#!usr/bin/perl
use Email::Address;
use HTML::Entities;
use HTML::Strip;
use warnings;
use strict;

sub decodeEmail() {
	my $msg = shift;
  my @lines;
  my $htmlStrip = HTML::Strip->new(
      emit_spaces => 0
    );
  my $body = $msg->body($msg->find_body);
  my $header = $msg->header;
  my $bodyStrippedOfHtml;
  my $bodyText;
  $body = $body->as_string;

  # testing purposes only
  # my $fileBefore = 'beforeDecode.txt';
  # my $fileAfter = 'afterDecode.txt';
  # open(my $fh1, '>', $fileBefore) or die "Could not open file '$fileBefore' $!";
  # open(my $fh2, '>', $fileAfter) or die "Could not open file '$fileAfter' $!";
  # print $fh1 "$body";
  # print $fh1 "\n*--- END OF EMAIL ---*\n";

  if($bodyStrippedOfHtml = isHtmlEncoded($header, $msg)) {
    # remove all HTML tags (brute force method using HTML::Strip)
    $bodyStrippedOfHtml = $htmlStrip->parse($body);
  }
  else {
    $bodyStrippedOfHtml = $body;
  }

  @lines = split /[\r\n]+/, $bodyStrippedOfHtml;
  foreach my $line (@lines) {
    # remove non-ASCII chars, whitespace and control chars
    $line =~ tr/\x00-\x1F//d;
    $line =~ tr/\x7B-\xFF//d;

    # find email addresses, dates, times, phone numbers and remove them
    $line = removeEmailAddresses($line);

    # begin removing other uneeded chars 
    # keep punctuation, apostrophe: . ! ? ' $
    $line =~ tr/\x22-\x23//d;
    $line =~ tr/\x25-\x26//d;
    $line =~ tr/\x28-\x2C//d;
    $line =~ tr/\x30-\x3E//d;
    $line =~ tr/\x5B-\x60//d;

    # replace chars that may join words with a space: - /
    $line =~ s/\// /g;
    $line =~ s/-/ /g;

    # ignore empty lines, undefined lines, and lines with only white-space
    if(! defined $line || $line =~ /^\s*$/ || $line eq ""){}
    else {
      # testing only
      # print "-*-" . $line . "\n";
      $bodyText .= $line . "\n";
    }
  }

  # testing purposes only
  # print $fh2 "$bodyText";
  # print $fh2 "\n*--- END OF EMAIL ---*\n";
  # close $fh1;
  # close $fh2;

	return $bodyText;
} # end sub decodeEmail

sub isHtmlEncoded() {
  my $header = shift;
  my $msg = shift;

  return getContentType($header, $msg) eq "text/html";
}

sub getContentType() {
  my $header = shift;
  my $msg = shift;

  if (not defined $header->{"content-type"}) {
    return "text/plain";
  }

  my $contentType = (split(/;/, $header->{"content-type"}))[0];
  if ($contentType =~ /multipart\/.+/ &&
      $msg->body(0) =~ /Content-Type: (?!multipart)([^;]+);/) {
    $contentType = $1;
  }

  return $contentType;
}

sub removeEmailAddresses() {
  my $line = shift;

  my @emailAddresses = Email::Address->parse($line);
  foreach my $address (@emailAddresses) {
    my $result;
    my $len;
    # special case: find trailing chars after top-level domain name
    if(  ($result = index($address,'.com') ) != -1
      || ($result = index($address,'.org') ) != -1
      || ($result = index($address,'.net') ) != -1
      || ($result = index($address,'.int') ) != -1
      || ($result = index($address,'.edu') ) != -1
      || ($result = index($address,'.gov') ) != -1
      || ($result = index($address,'.mil') ) != -1) {
        $len = length $address;
        $result = $result + 4;
        if($len > $result) {
          $address = substr $address, 0, $result;
      }
    }
    $line =~ s/$address//g;
  }
  return $line;
}

1;
