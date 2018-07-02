CS 5271 Project - NLP Phishing
——————————————————————————————
Nathan Anderson
Travis Carlson
Justin Peterson
Danielle Stewart

December 15, 2016

To run this program:
perl nlpPhishing.pl

The output contains classification for each email 
and the overall recall of the corpus.

Required modules:
————————————————— 
WordNet dictionary database (*) 
WordNet::QueryData 
WordNet::Tools 
WordNet::SenseRelate::AllWords 
Net::DNS
Email::Addresses
HTML::Entities
HTML::Strip
URI::Find
Main::MBoxParser

(*)Note: The WNHOME path variable should be set to point to 
the directory holding the WordNet dictionary. 
export WNHOME=‘path/to/wordnet/version#’
If this variable is set correctly, this system can access your word net
database without any issues. 

To change the corpus, in nlpPhishing.pl when the Mail::MBoxParser 
is created, pass it the name of the corpus you wish to run. 
Spam Corpus: ‘corpus.mbox’
Legitimate Corpus: ‘corpus_not_spam.mbox’
Random subset of spam corpus that has been edited: ‘random_corpus.mbox’

As is, the code will run the spam corpus. It takes approximately 30 minutes
to run through these emails. You will see output for each email that 
completes the testing. 

If you wish to implement the header attack, change the parameter in 
nlpPhishing “$breakHeader” to 1. 
If you wish to run the classification as usual, leave the parameter
as 0. 
(See comments in nlpPhishing.pl)

The main algorithm is as follows:
For every email:
  header h = extractHeader()
  links = getLinks()
  userDomain = getUserDomain()
  if email is html encoded 
    then decode email
  headerScore = headerAnalysis(userDomain, header)
  linkScore = linkAnalysis(decodedEmail)
  textScore = textAnalysis(decodedEmail)
  combinedScores = combineScores(headerScore, linkScore, textScore)
  if combinedScore >= 1.5
    output phishing
  else 
    output legitimate

To see details on this algorithm and the components 
(header, text, link), see comments in each subroutine.

