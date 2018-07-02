# fillHyponymArray
#
# This subroutine takes in one argument, a reference to an array of words. 
#
# This subroutine requires the WordNet::QueryData module and the WordNet dictionary. 
# The WNHOME path variable should be set to point to the directory holding 
# the WordNet dictionary. 
# If this variable is set correctly, the hardcoded pathname passed into the 
# QueryData constructor is unnecessary. 
# See: http://search.cpan.org/dist/WordNet-QueryData/QueryData.pm
#
# It will go through the array and for each word, push onto a new hyponym array
# the corresponding hyponym set. 
# The return value is a reference to the array holding the hyponym set. 
#


#!/usr/bin/perl

use warnings;
use strict;
use IO::Handle;
use WordNet::QueryData;

sub fillHyponymArrays {

  # Get hash reference passed in by processNouns
  my $hypHash;
  
  # Hash to send back to processNouns
  my %synset;
  
  # a temporary array used to hold return value from QueryData
  my @query = ();
  
  # A temporary array used to split word returned from query data
  my @temp = ();
  
  # Get arguments
  if(@_) {
    $hypHash = $_[0];
  } else {
    print "Need hyponym hash ref (fillHyponymHash).\n";
    exit 0;
  }


  # The WordNet QueryData object construction
  my $queryData = WordNet::QueryData->new();
    defined $queryData or die "Construction of WordNet::QueryData object failed.\n"; 
  
  
  # Loop through and remove any underscores and push hyponym synset 
  # onto a new array 
  foreach my $key (keys %{$hypHash}) {
    
    # Create a string to pass into queryData
    # and then collect the hyponym set in @query
    my $temp1 = $key;
    my $temp2 = ${$hypHash}{$key};
    my $temp = "$temp1#n#$temp2";
    
    # Get the hyponym set and put it into array
    @query = $queryData->querySense($temp, "hypo"); 
    
    # foreach word returned from queryData, 
    # put it into synset hash to return
    # Organized as {"word" => senseIDNumber}
    foreach(@query) {
      
      @temp = split /\#/,$_;
      $synset{$temp[0]} = $temp[2];
      
    } # end foreach @query
  } # end for my keys in hypHash
  
  
  return \%synset;

} # end sub fillHyponymArrays
1;
