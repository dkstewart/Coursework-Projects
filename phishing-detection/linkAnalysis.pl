#!usr/bin/perl
use warnings;
use strict;

sub linkAnalysis() {
	my $links = shift;
	my $blacklistedDomains = shift;

	foreach my $link (@{ $links }) {
		my $domain = domainOf($link);

		my @domainFields = split(/\./, $domain);

		while (@domainFields) {
			my $tryDomain = join(".", @domainFields);
			if (defined ${$blacklistedDomains}{$tryDomain}) {
				return 1;
			}
			shift @domainFields;
		}
	}
	return 0;

} # end sub linkAnalysis

sub domainOf() {
	my $url = shift;

	# strip off everything up to first :
	$url =~ s/^[^:]+://;
	# strip off everything after ?
	$url =~ s/\?.*//;
	# strip leading /
	$url =~ s/^\/+//;
	# strip everything after /
	$url =~ s/\/.*//;

	return $url;
}

sub getBlacklistedDomains() {
	my %blacklistedDomains = ();

	open(my $fh, "<", "blacklist.txt") or die "couldn't open blacklist.txt";
	foreach my $line (<$fh>) {
		chomp $line;
		$line =~ s/#.*//;
		$line =~ s/^\s+//;
		$line =~ s/\s+$//;
		if ($line ne "") {
			$blacklistedDomains{$line} = 1;
		}
	}

	return %blacklistedDomains;
}

1;
