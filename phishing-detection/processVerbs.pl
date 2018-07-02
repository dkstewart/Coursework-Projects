# processVerbs
#
# Author: Danielle Stewart
#
# 
# Subroutine takes in four arguments: 
# (1) The text file to be read from (the email)
# (2) The boolean value whether or not money was found in the 
# processNouns subroutine. 
# (3) queryData object reference (WordNet::QueryData)
# (4) wsd object reference (WordNet::SenseRelate::AllWords)
#
#
# We create a hash of urgency words (%urgency). This includes
# words like (now, today, desperately, immediately,..). 
# We also create a set of reply inducing words, this includes things
# like (write, reply, respond, click, go, update, submit, ...). 
#
# The hash of reply words are assigned with a pos tag and a sense id number. 
# The words look like "write=>1" where the word is the key and the sense is 
# the value (manually assigned from WordNet).
#
# The set of reply words are used to create a synset (cognitive synonyms)
# using the WordNet::QueryData object. This will give us a set of 
# words and senses that are synonyms to the words in %reply. 
#
# We now have three sets: %reply, %urgency, and %synset. 
#
# We check each sentence from the email (this is done using a helper
# subroutine called testSentences). 
# 
# If a word from %reply is in the email, we check to see if it has the
# same sense id. If so, we have a match. Set s = 1 and L = 1. 
#    If there is also a word from %urgency in that same sentence
#    then set the value of u = 1 and save this scoring (m+s+u)/2^L
#  
#    Else, save the score with u = 0: (m+s+u)/2^L
#
# Else check to see if there is a word from %synset in the email. 
# If there is, then we check to see if it has the same sense id. 
# If so, we have a match. Set s = 1 and L = 2. 
#    If there is also a word from %urgency in that same sentence, 
#    then set the value of u = 1 and save the scoring (m+s+u)/2^L
#
# Notice that L carries a value of one if the email word is found
# in the first "level" of words (%reply) and L = 2 if the email word
# is found in the second "level" (%synset). 
#
# We do this for every sentence in the email. All nonzero scores that are 
# collected are saved in an array @verbScores. 
#
# The return value of this subroutine is max(verbScores).
# 
# Even though this routine can calculate scores greater than 1
# we only return a max of score = 1. 
# During testing, we found that when textAnalysis is the only
# component that scores an email as phishing (header and link do not)
# it is not as accurate as when two or more components classify
# the email as phishing. 
# This is why we return only 1 as a max score. 


#!/usr/bin/perl

use warnings;
use strict;
use IO::Handle;
use List::Util qw( max );

require 'testSentences.pl';


sub processVerbs {

  # emailText is the text passed in from textAnalysis. This is the plain text email. 
  # $m is the value of moneyFound (from processNouns)
  # queryData is the WordNet QueryData object construction
  # wsd is the WordNet SenseRelate AllWords object construction
  my ($emailLines, $m, $queryData, $wsd);
  
  # Make sure arguments are okay
  if(@_) {
    ($emailLines, $m, $queryData, $wsd) = @_;

  } else {
    print "Need text file for argument (processVerbs).\n";
    exit 0;
  } 
  
  # List of urgency words (the set U)
  # All senses are set to one in the hash, these words do not have
  # a sense (they are mostly adverbs), but I needed a value to go
  # with the hash key. So one it is... 
  my %urgency = ("now"=>1,"today"=>1,"instantly"=>1,"soon"=>1,"straightaway"=>1,
  		 "directly"=>1,"once"=>1,"urgently"=>1,"desperately"=>1,"immediately"=>1,
  		 "shortly"=>1,"quickly"=>1,"nowadays"=>1,"presently"=>1,
  		 "straight"=>1,"forthwith"=>1,"within"=>1,"inside"=>1,
  		 "before"=>1,"ahead"=>1,"front"=>1);
  		 
  
  # List of reply words (the set R)
  my %reply = ("write"=>1,"write"=>3,"contact"=>1,"reply"=>1,"reply"=>2,
  				 "respond"=>1,"respond"=>2,"forward"=>1,"send"=>3,"hear"=>2, 
  				 "click"=>3, "follow"=>2,"follow"=>5,"visit"=>1,"go"=>1,
  				  "go"=>8,"update"=>2,"apply"=>1,"apply"=>3,"submit"=>1,
  				  "cancel"=>1,"dispute"=>1,"enroll"=>1);
  	       
  	# Synset is the words retrieved from the wordnet query returning a synset of each
  	# word in the reply array.
  	my @synset = ();
  	
  	# synsetHash will be the organized version of what came back from wordnet query
  	my %synsetHash = ();
  
  # Sentences from the email to be tested later
  my @sentences = ();
  
  # Holds all scores of each verb in the email. We save
  # the value 0 (verb not in reply or urgency lists)
  # For the case when there is nothing found in the email.
  # Then we send zero back to textAnalysis. 
  my @verbScores = ();
  push @verbScores, $m;
  my $score = 0;
  
 
  
  # Used to calculate score: 
  # s = 1 if word found in email that is in the synset of @reply
  #	u = 1 if urgency word is in the same sentence as response word
  #	L = 1,2 depending on whether word was in the set @reply (1) or @synset (2)
  # All are zero otherwise.
  my ($s, $u, $L) = (0,0,0);

  	# Get the synset for each word in the reply hash
  	for my $key (keys %reply) {
  	
			# the synset of the words in @reply (Synset(R))
			push @synset, $queryData->querySense($key, "syns");
			
			# GO through the synset array and convert it to a hash
			# where the key is the word, value is the sense id
			foreach(@synset) {
			
			  # Split the word in synset at the '#' character
			  # The elements in this array are "word#n#1"
			  my @temp = split /\#/,$_;
			  
			  # Set word as key and sense id as value
			  $synsetHash{$temp[0]} = $temp[2];
			
			} # end foreach @synset	
	} # end for each word in reply hash
  	
  # Now all our hashes are populated (%reply, %synset, %urgency)
  # and we can begin the search through the email. 
  
  # First we take the email text and separate out the sentences. I assume all 
  # sentences end with . ! ?
  @sentences = split(/\.|\?|!/, join(" ", @{ $emailLines }));
  
  # For each sentence in the array, check for words from the three hashes. 
  foreach(@sentences) {
  
    # Remove weird characters
    $_ =~ s/\.|,|"|\^|\$|\*|\+|\?|\(|\)|\[|\{|\\|\||\/|_|\-|\+|\]/ /g;
  
    # Send the hash %reply along with the sentence array to the
    # subroutine testSentences
    # The return value will be the value of $s
    my @temp = split /\s+/, $_;
    $s = testSentences($wsd, \%reply, \@temp, 0);

    # Now if $s == 0, that means there were no words
    # from the reply array that we found. So we check 
    # the synset array.
    if($s == 0) {
      
      # Send the synset array over to testSentences
      $s = testSentences($wsd, \%synsetHash, \@temp, 0);
      
      # If s == 1 now, that means our level is 2. 
      # Set the level and send urgency array over to testSentences
      if($s == 1) {
        
        # we are in the second level - test for urgency words
        $L = 2;
        $u = testSentences($wsd, \%urgency, \@temp, 1);
        
        # Regardless of the value of u, we save the score of the verb
        $score = ($m+$s+$u)/2**$L;
        push @verbScores, $score;
        
      } # end s == 1 and L == 2
    } # end if s==0
    
    # else $s == 1 so we check urgency on that same sentence and set level to 1
    else {
    
      # Set L = 1
      $L = 1;
      
      # Send @urgency over to testSentences
      $u = testSentences($wsd, \%urgency, \@temp, 1);
      
      # Calculate the score and save it in the array of scores
      $score = ($m+$s+$u)/2**$L;
      push @verbScores, $score;
      
    } # end else s == 1
  } # end foreach sentence
  
  # Return the max value we got from the scoring process
  if(max @verbScores > 1){
   return 1;
  }
  else{
   return max @verbScores;
  }
  

}
1;
