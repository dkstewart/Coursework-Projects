# testSentences
#
# Author: Danielle Stewart
#
# Subroutine takes in four arguments: 
# (1) The wsd object from WordNet::SenseRelate::AllWords
# (2) A reference to the hash to test (%reply, %urgency, %synset)
# (3) A reference to the sentence from the email (hash form)
# (4) A flag telling us if we are looking at the urgency hash
#     I needed a flag because if we are looking at urgency, we
#     will not be looking at the sense. We will just flag the word
#     as it is found in the email. 
#
# First we initialize variables.
# Then we will go through each word in the sentence passed in. 
#
# If the sentence word is in the hash (%reply, %urgency, 
# %synset), then we disambiguate the sentence text
# and test the sense id. If the sense id's match, then we return true. 
# Else, we keep searching for a new word. 
#
# If no words are found in that sentence, we return 0. 
#  

#!/usr/bin/perl

use warnings;
use strict;
use IO::Handle;

sub testSentences {

  # Arguments are the hash to test (the reply/urgency/synset words)
  # The sentence to test against (from the email) 
  # And a flag telling us if we need to check sense id's.
  # The urgency hash consists of words that do not have sense id's (mostly adverbs)
  my $hash;
  my $sentence;
  my $flag = 0;
  # The WordNet SenseRelate AllWords object construction
	my $wsd;
  
  # Return value tells processVerbs whether or not the word was found (1 or 0)
  my $value = 0;
  
  # temp array to store wsd results (after splitting at the '#' character
  my @temp = ();
  
  # Get arguments from command line
  if(@_) {
    ($wsd, $hash, $sentence, $flag) = @_;
  } else {
    print "Need hash references (testSentence).\n";
    exit 0;
  }
  
  # For each word in the hash (reply/urgency/synset), 
  # check to see if it matches anything in the sentence.
  # If there is a match we disambiguate the sentence words and see if the 
  # senses match. 
  # If not, we keep looking. 
  # If there is no match, we return 0.
  
  # We use a temp array to store wsd results
  my @wsdSentence = ();
  
  OUTER: 
  foreach my $sentenceWord (@{$sentence}) {
  
    # If the word is in the hash...
    if(exists ${$hash}{lc $sentenceWord}) {
    
      # Disambiguate the sentence to get the sense of that word
      push @wsdSentence, $wsd->disambiguate(window => 3, context => [@{$sentence}]);
      
      # For each of the wsd words, save by splitting at the '#' symbol
      foreach my $wsdSentenceWord (@wsdSentence) {
      
        # push into temp array
        @temp = split /\#/, $wsdSentenceWord;
        
        # When weird formatted words come in to wsd, 
        # there is no sense given. 
        # So I set the value $temp[2] = 0 so that the
        # test below doesn't cause an error
        if(! exists $temp[2]) {
          $temp[2] = 0;
        } # if temp[2] doesn't exist
        
        # if this word exists in the hash, then we found the word in question
        if(exists ${$hash}{$temp[0]}) {
        
          # Now check the flag to see if we need to check senses
          if($flag == 0 && ${$hash}{$temp[0]} == $temp[2]) {
          
            # Set value to 1 and jump out of loop
            $value = 1;
            last OUTER;
          
          } # end flag is zero and senses are equal
          
          # else flag is one, return 1 (don't check senses on urgency hash)
          else {
            
            # Set value to 1 (don't have to check senses for urgency hash)
            $value = 1;
            last OUTER;
            
          } # end if flag is 1
        } # end if word in hash
      } # end foreach @wsdSentence 
    } # end if word is in hash
  } # end foreach word in sentence 
  return $value;
    
}
1;
