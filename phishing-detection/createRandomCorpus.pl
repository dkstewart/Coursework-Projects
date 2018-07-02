#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(shuffle);

# usage: ./createRandomCorpus.pl [CORPUS_SIZE] [NEW_CORPUS_FILENAME]

my $CORPUS_FILENAME = "20050311_spam_2.tar.bz2";
my $CORPUS_LINK = "https://spamassassin.apache.org/publiccorpus/$CORPUS_FILENAME";
my $CORPUS_DIR = "spam_2";
my $MAX_CORPUS_SIZE = 180;
my $DEFAULT_OUT_FILENAME = "random_corpus.mbox";

# MAIN
{
	my ($corpus_size, $out_filename) = parse_args(@ARGV);
	download_corpus();

	open(my $fh, ">", $out_filename) or die "can't open $out_filename: $!";

	print "creating $out_filename of size $corpus_size\n";
	foreach my $i ((shuffle(1..$MAX_CORPUS_SIZE))[0..$corpus_size-1]) {
		write_email($fh, $i);
	}

	close($fh);
}

sub write_email
{
	my $fh = shift;
	my $i = shift;

	my $cmd = sprintf("cat $CORPUS_DIR/%05d.*\n", $i);

	open(my $pipe, "$cmd |") or die "can't run $cmd: $!";
	foreach my $line (<$pipe>) {
		print $fh $line;
	}
	close($pipe);
}

sub parse_args
{
	my $corpus_size = $#_ < 0 ? $MAX_CORPUS_SIZE : shift;
	my $out_filename = $#_ < 0 ? $DEFAULT_OUT_FILENAME : shift;

	if ($corpus_size !~ /^\d+$/) {
		$corpus_size = 0;
	}

	return ($corpus_size, $out_filename);
}

sub download_corpus
{
	if (! -d $CORPUS_DIR) {
		if (! -e "$CORPUS_FILENAME") {
			run_cmd("wget $CORPUS_LINK");
		}
		run_cmd("tar -xjf $CORPUS_FILENAME");
	}
}

sub run_cmd
{
	my $cmd = shift;
	print "$cmd\n";
	system $cmd and die "command returned non-zero: $cmd: $!";
}

