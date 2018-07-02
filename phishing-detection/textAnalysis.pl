# textAnalysis
#
# Author: Danielle Stewart
#
# This subroutine takes in the plain text email to be tested.
# 
# we will call on the subroutine processNouns. It will go through the email
# and determine if there was a mention of money or money
# related words. 
#
# If so, it will return a nonzero value $moneyFound (1 if found).
# If there was no mention of money, it returns zero. 
# (For more information on how this subroutine works, see it's documentation.)
#
# Then we call on the processVerbs subroutine. 
# This will check each sentence in the email for words associated with urgency or words that
# induce a response. It will take in the email text and the moneyFound value. 
# processVerbs will calculate an overall score for the email based on it's findings. 
# (For more information about scoring, see documentation on processVerbs.)
#
# Once we have a return value from processVerbs, textAnalysis is done. We return that 
# value to nlpPhishing. 
#
# The subroutine also defined in this file (initTextAnalysis) will
# create all objects needed to access WordNet, perform word sense
# disambiguation, and will create the money words set using WordNet. 
# This was used to save time in running the entire program - creation of these
# objects is nontrivial when it is performed for every email in the corpus.
#
# This subroutine requires WordNet::QueryData, WordNet::Tools, and 
# WordNet::SenseRelate::AllWords.
# This subroutine requires the WordNet::QueryData module *and* the WordNet dictionary. 
# The WNHOME path variable should be set to point to the directory holding 
# the WordNet dictionary. 
# export WNHOME='path/to/wordnet/version#'
# See: http://search.cpan.org/dist/WordNet-QueryData/QueryData.pm
#
# The process for getting money words is as follows:
# We first define a set of money related words. 
# moneyWords = (bank, money, cash, account, credit, check, funds,
# 		finance, gold, dollar,...)
#
# This is manually tagged with senses using WordNet database. 
# We take this set of words and get a list of hyponyms from WordNet. 
# A hyponym relation is as follows:
# X is a hyponym of Y if X is a kind of Y. 
# So spoon is a hyponym of silverware. 
#
# Once we get the synset (a set of hyponyms from WordNet) from the
# set of money words, we then take this set (I call it level 1) of hyponyms
# and get another set (level 2). We do this four times. 
# We now have four levels of hyponyms. Since the algorithm described
# doesn't keep track of what level the words are in when they are found 
# (unlike the verb processing portion) we push all these into one
# array called @hyponyms. 

#!usr/bin/perl
use warnings;
use strict;
use IO::Handle;

# Subroutines used by textAnalysis
require 'processNouns.pl';
require 'processVerbs.pl';
require 'fillHyponymArrays.pl'; 

sub textAnalysis() {

  # The email text
  my $parsedEmail = shift;
  
  # The WordNet QueryData object 
  my $queryData = shift;
  
  # The WordNet SenseRelate AllWords word sense disambiguation object
  my $wsd = shift;
  
  # The money words hash
  my $moneyHyponyms = shift;
  
  # Split the email at each line for further analysis
  my @emailLines = split(/\n/, $parsedEmail);
  
  # Boolean value for whether or not money was found
  my $moneyFound = 0;
  
  # The final score that will be returned to nlpPhishing
  my $verbScore = 0;

  # processNouns looks at money words
  $moneyFound = processNouns(\@emailLines, $wsd, $moneyHyponyms);
  
  # processVerbs will do all kinds of nifty things with the verbs in the email
  # This really focuses on words associated with urgency and words that attempt to 
  # induce a response. 
  $verbScore = processVerbs(\@emailLines, $moneyFound, $queryData, $wsd);

  return $verbScore;

} # end sub textAnalysis


# initTextAnalysis
# Parameters: None
# Return values: 
#   WordNet access object (queryData)
#   WSD object (wsd)
#   Hyponym hash of money words
# Initializes objects and creates hash of money words
# and their hyponymy links using WordNet database. 
sub initTextAnalysis() {

  # Check to see if the WNHOME variable is set
  # If it is not, try to find the path to WordNet dictionary
  my $username = $ENV{"LOGNAME"} || $ENV{USER} || getpwuid($<);
  my $wordnet_dir = $ENV{'WNHOME'} // "/Users/$username/WordNet/WordNet-3.0";

  # Create the queryData object to access WordNet database
  my $queryData = WordNet::QueryData->new("$wordnet_dir/dict");
    defined $queryData or die "Construction of WordNet::QueryData object failed.\n";

  # The WordNet Tools object construction
  my $wntools = WordNet::Tools->new($queryData);
    defined $wntools or die "\nCouldn't construct WordNet::Tools object.\n";

  # The WordNet SenseRelate AllWords object construction
  my $wsd = WordNet::SenseRelate::AllWords->new (wordnet => $queryData,
						 wntools => $wntools,measure => 'WordNet::Similarity::lesk');
    defined $wsd or die "\nCouldn't construct WordNet::SenseRelate::AllWords object.\n";

  # Words associated with money
  my %moneyWords = ("bank" =>2, "money"=>1,"cash"=>1,"account"=>3,"credit"=>2,
  			"credit"=>5, "check"=>2,"fund"=>1,"finance"=>1, "gold"=>1, "dollar"=>1,
  		        "marketing"=>1, "mortgage"=>1, "rate"=>2,"rates"=>1, "free"=>3, "free"=>1,
  		        "profit"=>1, "profit"=>2, "stock"=>1);
    
  # Fill up the first level of hyponyms
  my $hyponymsL1 = fillHyponymArrays(\%moneyWords);
  
  # Fill up the second level
  my $hyponymsL2 = fillHyponymArrays($hyponymsL1);
  
  # And the third level
  my $hyponymsL3 = fillHyponymArrays($hyponymsL2);
  
  # And fourth level
  my $hyponymsL4 = fillHyponymArrays($hyponymsL3);
  
  # Push all these onto one hash called hyponyms
  my %hyponyms = (%moneyWords,%{$hyponymsL1});
  %hyponyms = (%hyponyms, %{$hyponymsL3});
  %hyponyms= (%hyponyms, %{$hyponymsL4});

  # Return the two objects created and a reference to the hyponym hash
  return ($queryData, $wsd, \%hyponyms);
}

1;
