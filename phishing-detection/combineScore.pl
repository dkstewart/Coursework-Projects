#!usr/bin/perl
use warnings;
use strict;

sub combineScore() {
	my $headerScore = shift;
	my $linkScore = shift;
	my $textScore = shift;

	return $headerScore + $linkScore + $textScore;
} #end sub combineScore
1;
