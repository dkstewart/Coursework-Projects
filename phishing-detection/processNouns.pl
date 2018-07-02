# Process Nouns
# Author: Danielle Stewart
# Subroutine in nlpPhishing.pl
#
# This subroutine takes in arguments containing the email in question,
# The word sense disambiguation object (WordNet::SenseRelate::AllWords), 
# and the hash reference of all money words (hyponym links - see initTextAnalysis). 
# This email is assumed to be in plain English text. 
#
# We first scan the email for currency symbols. If one is found, 
# then there is a mention of money and we are done.  
# If not, we check for words in the email that are in our money word hash.
# 
# If no money word is found in the email, we return 0 to textAnalysis. 
# If money words were found, the real work begins. We save all moneyWords in an 
# array called @emailMoneyWords. Then we loop through this array performing the
# following actions on each money word found in the email.
# 
# We first perform word sense disambiguation on the email word in question.
# This will tell us if the writer of the email was talking about (for instance)
# a river bank where they go fishing or a bank where they can access funds. 
#
# We find the line in the email where money word was used.
# We send that line into a word sense disambiguation object from 
# WordNet::SenseRelate::AllWords. This will return
# each word in the line with a sense attached. 
# This sense is in numerical form and matches the manual sense tags that
# were put on the original moneyWords list. 
#
# Then we check the sense of the word in the email with the sense of the
# word in the moneyWords list (or a hyponym list associated with money words).  
#
# If the senses are not equal, we go to the next word in the emailMoneyWords list.
# Once we have checked all money words found in the email and no senses match, 
# we return 0.  

#!/usr/bin/perl

use warnings;
use strict;
use IO::Handle;
use WordNet::QueryData;
use WordNet::Tools;
use WordNet::SenseRelate::AllWords;

sub processNouns {

  # Lines in the email text
  my @emailLines;

  # The WordNet SenseRelate AllWords object 
  my $wsd;

  # The hyponym set (hash reference)
  my $hyponyms;
  
  # Gather all the arguments
  if(@_) {
    @emailLines = @{ $_[0] };
    $wsd = $_[1];
    $hyponyms = $_[2];
  } else {
    print "Need text file for argument (processVerbs).\n";
    exit 0;
  } 
  
  # Word Sense Disambiguation for email words
  my @wsdResults = ();
  
  # Will hold words from the sentences in email
  my @sentence = ();
  
  # For every line in the email... 
  foreach my $line (@emailLines) {
  
    # Format line appropriately
    chomp($line);
    
    # Remove weird characters 
    $line =~ s/\.|\^|\*|\+|\?|\(|\)|\[|\{|\\|\||\/|_|\-|\+|\]|!//g;
    
    # Before we go through every word, we check to see if there is a 
    # currency symbol in the line. 
    # If so, our job is done. 
    if($line =~ /\$/) {
      
      # Send back yes, money found
      return 1;
      
    } # if there is a dollar symbol in the line
    
    # Put each word of the line into an array
    @sentence = split /\s+/,$line;
    
    # Go through every word in the sentence and look for money words
    foreach my $word (@sentence) {
    
      # If the word is in the hyponym hash...
      if(${$hyponyms}{$word}) {
      
        # disambiguate the sentence
        push @wsdResults, $wsd -> disambiguate (window => 3,
	 				    context => [@sentence]);
        
        # Now go through words in the wsd results and 
        # check to see if we can match senses with hyponym word
        foreach(@wsdResults) {
        
          # Save word#n#1 as word, n, 1 in array
          my @tempArray = split/\#/,$_;
          
          # Find the word in our hyponym hash
          if(${$hyponyms}{$tempArray[0]}) {
          
            # Test senses
            if($tempArray[2] == ${$hyponyms}{$tempArray[0]}) {
            
              # Done, we found what we were looking for
              return 1;
            
            } # end if testing sense id's
          } # end if word in hyponym hash
        } # end foreach @wsdResults
      } # end if word in hyponym hash
    } # end foreach word in sentence
  } # end while line in email
  
  # If we haven't returned yet, that means we found nothing. 
  return 0;
  
} # end processNouns
1;
