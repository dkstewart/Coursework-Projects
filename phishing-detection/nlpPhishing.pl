# nlpPhishing.pl is the main routine that
# will call all other classification components. 
# It also outputs the result for the user. 

#!/usr/bin/perl
use warnings;
use strict;
use IO::Handle;
use Mail::MboxParser;
use URI::Find;

# The list of subroutines included in this system
require 'decodeEmail.pl';
require 'headerAnalysis.pl';
require 'linkAnalysis.pl';
require 'textAnalysis.pl';
require 'combineScore.pl';

# Main holds the main algorithm.
# The other subroutines will perform the necessary parsing, analysis, and calculations.
sub main {
	my @arguments = @ARGV;
  my $decodedMsg;
  
  # To only run text analysis when another
  # component gives a score of 1, set this variable to 0. 
  # If you wish to always run text analysis, set it to 1.
	my $alwaysRunTextAnalysis = 1;
	
	# To perform attack on header, change this variable to 1. 
	# Else, set to zero. 
	my $breakHeader = 0;

	# TODO: pass user's email domain as argument?
	my $userDomain = 'netnoteinc.com';

	my $parseropts = {
		enable_cache    => 0,
		enable_grep     => 1,
	};
	
	# The corpus used to run this system is passed into the 
	# parser here. 
	# Corpus of legitimate emails: 'corpus_not_spam.mbox'
	# Corpus of spam emails: 'corpus.mbox'
	# Subset corpus of edited emails: 'random_corpus.mbox'
	my $mb = Mail::MboxParser->new('corpus.mbox',
					decode     => 'ALL',
					parseropts => $parseropts);

  # queryData is the WordNet QueryData object construction
  # wsd is the WordNet SenseRelate AllWords object construction
  # moneyHyponyms are all money words we search for in textAnalysis
	my ($queryData, $wsd, $moneyHyponyms) = initTextAnalysis();
	
	# The blacklisted URLs used in linkAnalysis
	my %blacklistedDomains = getBlacklistedDomains();
	
	# Parameters used to output results
	my $linkFailCnt = 0;
	my $headerFailCnt = 0;
	my $textFailCnt = 0;
	my $phishingCnt = 0;
	my $legitimateCnt = 0;
	my $textScore = 0;

  # For every email message we have in the mbox file...
	foreach my $msg ($mb->get_messages) {
    
    # extract header and body of message
    my $header = $msg->header;
    my $body = $msg->body($msg->find_body);

    # extract links
    # this will also grab 'mailto' links, not just 'http'
    my @links;
    # set finder and callback function
    my $finder = URI::Find->new(sub {
      my($uri) = shift;
      # if mailto then get domain
      if (index($uri,'mailto') != -1) {
        my $result = index($uri,'@') + 1;
        $uri = substr $uri, $result;
      }
      push @links, $uri;
    });
    
    # parse body text
    $finder->find(\$body);

    # strip body of html and special chars
    $decodedMsg = decodeEmail($msg);

    # Get the link score
		my $linkScore = linkAnalysis(\@links, \%blacklistedDomains);
		
		# Get the header score
		my $headerScore = headerAnalysis($userDomain, $header,
			$msg->from_line, $msg->get_field('received'), $breakHeader);
			
		# text analysis is only necessary if current score is 1,
		# but we might want to know the result anyway
		if ($alwaysRunTextAnalysis || ($linkScore + $headerScore == 1)) {
			$textScore = textAnalysis($decodedMsg, $queryData,
							$wsd, $moneyHyponyms)
		}

    # And the combined score from all three components
		my $cs = combineScore($headerScore, $linkScore, $textScore);

    # Update counters for output
		++$linkFailCnt if $linkScore >= 1;
		++$headerFailCnt if $headerScore >= 1;
		++$textFailCnt if $textScore >= 0.5;

    # If the combined score is >= 1.5, we have phishing
    # Else, it's legitimate
		if ($cs >= 1.5) {
			outputPhishing($header);
			++$phishingCnt;
		} else {
			outputLegitimate($header);
			++$legitimateCnt
		}
		
		# Print output
		print "\tlinkScore: $linkScore\n";
		print "\theaderScore: $headerScore\n";
		print "\ttextScore: $textScore\n";
	}

  # Print output on the entire corpus results
	my $totalEmailCnt = $phishingCnt + $legitimateCnt;
	if ($totalEmailCnt != 0) {
		printf "link   failures : %d / %d (%.3f%%)\n",
			$linkFailCnt,
			$totalEmailCnt,
			100 * $linkFailCnt / ($totalEmailCnt);

		printf "header failures : %d / %d (%.3f%%)\n",
			$headerFailCnt,
			$totalEmailCnt,
			100 * $headerFailCnt / ($totalEmailCnt);

		printf "text   failures : %d / %d (%.3f%%)\n",
			$textFailCnt,
			$totalEmailCnt,
			100 * $textFailCnt / ($totalEmailCnt);

		print "\n";
		printf "phishing emails : %d / %d (%.3f%%)\n",
			$phishingCnt,
			$totalEmailCnt,
			100 * $phishingCnt / ($totalEmailCnt);
	}
} # end main

# These two subroutines just print the header, subject, and classification
sub outputPhishing() {
	my $header = shift;
	print "phishing: ", $header->{subject}, "\n";
}

sub outputLegitimate() {
	my $header = shift;
	print "legitimate: ", $header->{subject}, "\n";
}

main;
