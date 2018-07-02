#!usr/bin/perl
use warnings;
use strict;
use Net::DNS;

sub headerAnalysis() {
	# example: umn.edu
	my $userDomain = shift;
	my $header = shift;
	# example: From drive-shares-noreply@google.com Mon Oct  3 13:35:41 2016
	my $fromLine = shift;
	my $receivedFromRawText = shift;
	my $break = shift;
	if (not defined $receivedFromRawText) {
		$receivedFromRawText = "";
	}

	# Phase 1 - Extracting the data
	# p.834 of Verma et al.
	# * Extract FROM and DELIVERED-TO fields from the header.
	# * If DKIM signature exists, store the Signing Domain ID (SDID).
	# * If SPF query returns "pass" and accepts IP address as permitted
	# 	sender, perform nslookup on the IP and store the domain.
	# * Store each RECEIVED FROM field in order.

	# extract domain (e.g. google.com) from $fromLine (see above)
	my $from = (split(/\s+/, $fromLine))[1];
	$from =~ s/.*@//;

	# example: [carl4980@g-mx.umn.edu, carl4980@umn.edu]
	my @deliveredTos = toArray($header->{'delivered-to'});

	# example: [oit.umn.edu, localhost, oit.umn.edu, google.com]
	my @receivedFroms = parseReceivedFroms($receivedFromRawText);

	# as noted in section 5.2 of Verma et al., a sophisticated phisher can
	# manipulate the Received From field completely, here we simulate a
	# phisher breaking the headerAnalysis by setting Received From to match
	# From
	if ($break) {
		$receivedFroms[0] = $from;
	}

	# extract the domain (e.g. google.com) from the dkim-signature if it exists
	my $sdid = "";
	# example $header->{'dkim-signature'}:
	# 	v=1; a=rsa-sha256; c=relaxed/relaxed; d=google.com; s=20120113; ...
	if (exists $header->{'dkim-signature'}
			&& $header->{'dkim-signature'} =~ /\Wd=([^;]+);/) {
		$sdid = $1;
	}

#	print "received-spf:[", $header->{'received-spf'}, "]\n";
	# lookup the domain of client-ip if SPF query returns "pass"
	# TODO: we should only store this if the received-from domain matches
	# $from. Note that our corpus does not contain SPF fields so this does
	# not affect our results.
	my $spfQuery = "";
	# example $header->{'received-spf'}:
	# 	pass (google.com: domain of 3hmh...pof@doclist.bounces.google.com
	#	designates 209.85.217.199 as permitted sender) client-ip=209.85.217.199;
	if (exists $header->{'received-spf'}
			&& $header->{'received-spf'}
				=~ /^pass.*\Wclient-ip=([\d.]+);$/) {
		my $clientIp = $1;
		# perform nslookup to get the hostname of client-ip
		$spfQuery = nslookup($clientIp);
		if (defined $spfQuery) {
			# drop the hostname, only want the domain name
			$spfQuery =~ s/^[^.]+\.//;
		}
	}

	# return score of 0 if legitimate; otherwise 1
	return isLegitimate($userDomain, $from, $sdid, $spfQuery, \@receivedFroms) ? 0 : 1;
} # end sub headerAnalysis

sub isLegitimate() {
	my $userDomain = shift;
	my $from = shift;
	my $sdid = shift;
	my $spfQuery = shift;
	my $receivedFromsRef = shift;
	my @receivedFroms = @{ $receivedFromsRef };

	# Phase 2 - Verifying the data
	# p.835 of Verma et al.
	# * If the first Received From field has the same domain name as the
	# 	FROM field or localhost, or any forwarding email account, then
	# 	this email is legitimate.
	# * If the nslookup on the IP address of the permitted sender in the
	# 	Received-SPF field yields the same domain name stored in the
	# 	variable SPFQuery, then this email is legitimate. (Isn't this
	# 	trivially true anytime SPFQuery is set? We set the SPFQuery
	# 	variable in Phase 1 to contain the domain name output from the
	# 	nslookup.)
	# * If the Received From field has the same domain name as the user's
	#	current email account's domain name, then look at the next
	#	received from field.
	# * Otherwise, mark the email as phishing.
	# * We don't look at the DELIVERED-TO fields or the SDID? Why did we
	#	store them?
	# * Answer: See "Two-Pronged Phish Snagging" paper
	# 	* if SDID exists, it should be checked INSTEAD of others
	# 	* if SPF exists, it should be checked IN ADDITION TO others
	# 	* still no answer about unused DELIVERED-TO fields
	# 		(maybe we're supposed check that they match $userDomain?)

	# if SDID exists, it should be checked INSTEAD of others
	if ($sdid ne "") {
		return $sdid eq $from;
	}

	foreach my $receivedFrom (@receivedFroms) {
		# assume no forwarding accounts in our case
		if ($receivedFrom eq $from || $receivedFrom eq 'localhost'
				|| $receivedFrom eq $spfQuery) {
			return 1;
		# mark as phishing if current Received From field does not have
		#	the same domain name as the user's current email account
		} elsif ($receivedFrom !~ /$userDomain$/) {
			return 0;
		}
	}

	return 0;
}

sub nslookup() {
	my $ip = shift;

	foreach my $ap (Net::DNS::Resolver->new->query($ip, 'PTR')->answer) {
		# FIXME: don't just return the first one found?
		return $ap->ptrdname;
	}

	return undef;
}

sub parseReceivedFroms() {
	my $receivedFromRawText = shift;
	my @receivedFroms;

	# extract domain name (e.g. oit.umn.edu) from lines like this:
	# Received: from mta-p14.oit.umn.edu (mta-p14.oit.umn.edu.
	#	[134.84.196.214]) by mx.google.com with ESMTPS id
	#	s81si665531ioi.16.2016.10.03.13.35.40 for <carl4980@g-mx.umn.edu>
	#	(version=TLS1_2 cipher=ECDHE-RSA-AES128-GCM-SHA256 bits=128/128); Mon,
	#	03 Oct 2016 13:35:41 -0700 (PDT)
	foreach my $line (split(/\n/, $receivedFromRawText)) {
		if ($line =~ /^Received: from ([^\s]+)/) {
			my $receivedFrom = $1;
			# drop the hostname, only want the domain name
			$receivedFrom =~ s/^[^.]+\.//;
			push(@receivedFroms, $receivedFrom);
		}
	}

	return @receivedFroms;
}

sub toArray() {
	my $x = shift;
	return ref($x) eq 'ARRAY' ? @{$x} : [$x];
}

1;
