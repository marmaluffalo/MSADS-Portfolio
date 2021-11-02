'''
  This program shell reads phrase data for the kaggle phrase sentiment classification problem.
  The input to the program is the path to the kaggle directory "corpus" and a limit number.
  The program reads all of the kaggle phrases, and then picks a random selection of the limit number.
  It creates a "phrasedocs" variable with a list of phrases consisting of a pair
    with the list of tokenized words from the phrase and the label number from 1 to 4
  It prints a few example phrases.
  In comments, it is shown how to get word lists from the two sentiment lexicons:
      subjectivity and LIWC, if you want to use them in your features
  Your task is to generate features sets and train and test a classifier.

  Usage:  python classifyKaggle.py  <corpus directory path> <limit number>
'''
# open python and nltk packages needed for processing
import os
import sys
import random
import nltk
from nltk.corpus import stopwords
from nltk.collocations import *
from nltk.tokenize import RegexpTokenizer
from nltk.tokenize import WordPunctTokenizer
import re
import sentiment_read_LIWC_pos_neg_words

#############################
## get words and  features ##
#############################

def get_words(docs):
  all_words = []
  for (words, sentiment) in docs:
    # more than 3 length
    # possible_words = [x for x in words if len(x) >= 3]
    # all_words.extend(possible_words)
    all_words.extend(words)
  return all_words

## another one but filter for >= 3 word sentences
def get_words_3(docs):
  all_words = []
  for (words, sentiment) in docs:
    # more than 3 length
    words3 = [x for x in words if len(x) >= 3]
    all_words.extend(words3)
  return all_words

## modified for the test document
def get_test_words(lines):
  all_words = []
  for id,words in lines:
    all_words.extend(words)
  return all_words

def get_word_features(wordlist):
  wordlist = nltk.FreqDist(wordlist)
  word_features = [w for (w, c) in wordlist.most_common(200)] 
  return word_features

#########################
## baseline featureset ##
#########################

# define features (keywords) of a document for a BOW/unigram baseline
# each feature is 'contains(keyword)' and is true or false depending
# on whether that keyword is in the document
def document_features(document, word_features):
    document_words = set(document)
    features = {}
    for word in word_features:
        features['V_{}'.format(word)] = (word in document_words)
    return features

####################
## pre processing ##
####################

## some stopwords
stopwords = nltk.corpus.stopwords.words('english')
newstopwords = [word for word in stopwords if word not in ['not', 'no', 'can','don', 't']]

def preprocessing(doc):
  # make all the words lowercase
  word_list = re.split('\s+', doc.lower())
  # remove puncutation and numbers
  punctuation = re.compile(r'[-.?!/\%@,":;()|0-9]')
  word_list = [punctuation.sub("", word) for word in word_list] 
  final_word_list = []
  for word in word_list:
    if word not in newstopwords:
      final_word_list.append(word)
  line = " ".join(final_word_list)
  return line 

#############################
## subjectivity featureset ##
#############################

def readSubjectivity(path):
  flexicon = open(path, 'r')
  # initialize an empty dictionary
  sldict = { }
  for line in flexicon:
    fields = line.split()   # default is to split on whitespace
    # split each field on the '=' and keep the second part as the value
    strength = fields[0].split("=")[1]
    word = fields[2].split("=")[1]
    posTag = fields[3].split("=")[1]
    stemmed = fields[4].split("=")[1]
    polarity = fields[5].split("=")[1]
    if (stemmed == 'y'):
      isStemmed = True
    else:
      isStemmed = False
    # put a dictionary entry with the word as the keyword
    #     and a list of the other values
    sldict[word] = [strength, posTag, isStemmed, polarity]
  return sldict

SLpath = "./SentimentLexicons/subjclueslen1-HLTEMNLP05.tff"
SL = readSubjectivity(SLpath)

def SL_features(document, word_features, SL):
  document_words = set(document)
  features = {}
  for word in word_features:
    features['contains({})'.format(word)] = (word in document_words)
  # count variables for the 4 classes of subjectivity
  weakPos = 0
  strongPos = 0
  weakNeg = 0
  strongNeg = 0
  for word in document_words:
    if word in SL:
      strength, posTag, isStemmed, polarity = SL[word]
      if strength == 'weaksubj' and polarity == 'positive':
        weakPos += 1
      if strength == 'strongsubj' and polarity == 'positive':
        strongPos += 1
      if strength == 'weaksubj' and polarity == 'negative':
        weakNeg += 1
      if strength == 'strongsubj' and polarity == 'negative':
        strongNeg += 1
      features['positivecount'] = weakPos + (2 * strongPos)
      features['negativecount'] = weakNeg + (2 * strongNeg)
  
  if 'positivecount' not in features:
    features['positivecount']=0
  if 'negativecount' not in features:
    features['negativecount']=0      
  return features

###############################
## Negation words featureset ##
###############################

# this list of negation words includes some "approximate negators" like hardly and rarely
negationwords = ['no', 'not', 'never', 'none', 'nowhere', 'nothing', 'noone', 'rather', 'hardly', 'scarcely', 'rarely', 'seldom', 'neither', 'nor']

def NOT_features(document, word_features, negationwords):
    features = {}
    for word in word_features:
        features['V_{}'.format(word)] = False
        features['V_NOT{}'.format(word)] = False
    # go through document words in order
    for i in range(0, len(document)):
        word = document[i]
        if ((i + 1) < len(document)) and ((word in negationwords) or (word.endswith("n't"))):
            i += 1
            features['V_NOT{}'.format(document[i])] = (document[i] in word_features)
        else:
            features['V_{}'.format(word)] = (word in word_features)
    return features

#######################
## Bigram featureset ##
#######################

def get_bigram_features(tokens):
  bigram_measures = nltk.collocations.BigramAssocMeasures()
  # create the bigram finder on all the words in sequence
  finder = BigramCollocationFinder.from_words(tokens,window_size=3)
  ## optionally filter for frequency
  #finder.apply_freq_filter(6)
  # define the top 1000 bigrams using the chi squared measure
  bigram_features = finder.nbest(bigram_measures.chi_sq, 1000)
  return bigram_features[:500]

def bigram_document_features(document, word_features, bigram_features):
  document_words = set(document)
  document_bigrams = nltk.bigrams(document)
  features = {}
  for word in word_features:
    features['contains({})'.format(word)] = (word in document_words)
  for bigram in bigram_features:
    features['bigram({} {})'.format(bigram[0], bigram[1])] = (bigram in document_bigrams)    
  return features

#######################
##  POS featureset  ###
#######################

# this function takes a document list of words and returns a feature dictionay
#   it depends on the variable word_features
# it runs the default pos tagger (the Stanford tagger) on the document
#   and counts 4 types of pos tags to use as features

def POS_features(document, word_features):
    document_words = set(document)
    tagged_words = nltk.pos_tag(document)
    features = {}
    for word in word_features:
        features['contains({})'.format(word)] = (word in document_words)
    numNoun = 0
    numVerb = 0
    numAdj = 0
    numAdverb = 0
    for (word, tag) in tagged_words:
        if tag.startswith('N'): numNoun += 1
        if tag.startswith('V'): numVerb += 1
        if tag.startswith('J'): numAdj += 1
        if tag.startswith('R'): numAdverb += 1
    features['nouns'] = numNoun
    features['verbs'] = numVerb
    features['adjectives'] = numAdj
    features['adverbs'] = numAdverb
    return features

####################
## LIWC Sentiment ##
####################

# initialize positve and negative word prefix lists from LIWC 
#   note there is another function isPresent to test if a word's prefix is in the list
(poslist, neglist) = sentiment_read_LIWC_pos_neg_words.read_words()

def LIWC_features(doc, word_features,poslist,neglist):
  doc_words = set(doc) 
  features = {}
  for word in word_features:
    features['contains({})'.format(word)] = (word in doc_words) 
  poscount = 0
  negcount = 0
  for word in doc_words:
    if sentiment_read_LIWC_pos_neg_words.isPresent(word,poslist): 
      poscount += 1
    if sentiment_read_LIWC_pos_neg_words.isPresent(word,neglist): 
      negcount += 1
    features['positivecount'] = poscount
    features['negativecount'] = negcount
  if 'positivecount' not in features:
    features['positivecount'] = 0
  if 'negativecount' not in features:
    features['negativecount'] = 0 
  return features

#################################
## Hybrid Sentiment Featureset ##
#################################

def hybrid_features(document, word_features, SL, poslist, neglist):
  document_words = set(document)
  features = {}
  for word in word_features:
    features['contains({})'.format(word)] = (word in document_words)
  # count variables for the 4 classes of subjectivity
  weakPos = 0
  strongPos = 0
  weakNeg = 0
  strongNeg = 0
  poscount = 0
  negcount = 0

  for word in document_words:
    if word in SL:
      strength, posTag, isStemmed, polarity = SL[word]
      if strength == 'weaksubj' and polarity == 'positive':
        weakPos += 1
      if strength == 'strongsubj' and polarity == 'positive':
        strongPos += 1
      if strength == 'weaksubj' and polarity == 'negative':
        weakNeg += 1
      if strength == 'strongsubj' and polarity == 'negative':
        strongNeg += 1
    
    elif sentiment_read_LIWC_pos_neg_words.isPresent(word,poslist): 
      poscount += 2
    elif sentiment_read_LIWC_pos_neg_words.isPresent(word,neglist): 
      negcount += 2

    features['positivecount'] = weakPos + (2 * strongPos) + (2 * poscount)
    features['negativecount'] = weakNeg + (2 * strongNeg) + (2 * negcount)
  
  if 'positivecount' not in features:
    features['positivecount'] = 0
  if 'negativecount' not in features:
    features['negativecount'] = 0      
  return features

########################
## calculate accuracy ##
########################

def calculate_accuracy(featuresets):
  print("Training and testing a classifier ")

  training_size = int(0.6*len(featuresets))
  test_set = featuresets[:training_size]
  training_set = featuresets[training_size:]
  classifier = nltk.NaiveBayesClassifier.train(training_set)

  print("Accuracy of classifier : ")
  print(nltk.classify.accuracy(classifier, test_set))
  print("---------------------------------------------------")
  print("Showing most informative features")
  print(classifier.show_most_informative_features(5))
  print("---------------------------------------------------")
  
  ## create confusion matrix
  goldlist = []
  predictedlist = []

  for (features, label) in test_set:
    goldlist.append(label)
    predictedlist.append(classifier.classify(features))
  
  cm = nltk.ConfusionMatrix(goldlist, predictedlist)
  print(cm.pretty_format(sort_by_count=True, show_percents=False, truncate = 9)) 

######################
## Cross Validation ##
######################

# this function takes the number of folds, the feature sets and the labels
# it iterates over the folds, using different sections for training and testing in turn
#   it prints the performance for each fold and the average performance at the end
def cross_validation_PRF(num_folds, featuresets, labels):
    subset_size = int(len(featuresets)/num_folds)
    print('Each fold size:', subset_size)
    # for the number of labels - start the totals lists with zeroes
    num_labels = len(labels)
    total_precision_list = [0] * num_labels
    total_recall_list = [0] * num_labels
    total_F1_list = [0] * num_labels

    # iterate over the folds
    for i in range(num_folds):
        test_this_round = featuresets[(i*subset_size):][:subset_size]
        train_this_round = featuresets[:(i*subset_size)] + featuresets[((i+1)*subset_size):]
        # train using train_this_round
        classifier = nltk.NaiveBayesClassifier.train(train_this_round)
        # evaluate against test_this_round to produce the gold and predicted labels
        goldlist = []
        predictedlist = []
        for (features, label) in test_this_round:
            goldlist.append(label)
            predictedlist.append(classifier.classify(features))

        # computes evaluation measures for this fold and
        #   returns list of measures for each label
        print('Fold', i)
        (precision_list, recall_list, F1_list) \
                  = eval_measures(goldlist, predictedlist, labels)
        # take off triple string to print precision, recall and F1 for each fold
        '''
        print('\tPrecision\tRecall\t\tF1')
        # print measures for each label
        for i, lab in enumerate(labels):
            print(lab, '\t', "{:10.3f}".format(precision_list[i]), \
              "{:10.3f}".format(recall_list[i]), "{:10.3f}".format(F1_list[i]))
        '''
        # for each label add to the sums in the total lists
        for i in range(num_labels):
            # for each label, add the 3 measures to the 3 lists of totals
            total_precision_list[i] += precision_list[i]
            total_recall_list[i] += recall_list[i]
            total_F1_list[i] += F1_list[i]

    # find precision, recall and F measure averaged over all rounds for all labels
    # compute averages from the totals lists
    precision_list = [tot/num_folds for tot in total_precision_list]
    recall_list = [tot/num_folds for tot in total_recall_list]
    F1_list = [tot/num_folds for tot in total_F1_list]
    # the evaluation measures in a table with one row per label
    print('\nAverage Precision\tRecall\t\tF1 \tPer Label')
    # print measures for each label
    for i, lab in enumerate(labels):
        print(lab, '\t', "{:10.3f}".format(precision_list[i]), \
          "{:10.3f}".format(recall_list[i]), "{:10.3f}".format(F1_list[i]))
    
    # print macro average over all labels - treats each label equally
    print('\nMacro Average Precision\tRecall\t\tF1 \tOver All Labels')
    print('\t', "{:10.3f}".format(sum(precision_list)/num_labels), \
          "{:10.3f}".format(sum(recall_list)/num_labels), \
          "{:10.3f}".format(sum(F1_list)/num_labels))

    # for micro averaging, weight the scores for each label by the number of items
    #    this is better for labels with imbalance
    # first intialize a dictionary for label counts and then count them
    label_counts = {}
    for lab in labels:
      label_counts[lab] = 0 
    # count the labels
    for (doc, lab) in featuresets:
      label_counts[lab] += 1
    # make weights compared to the number of documents in featuresets
    num_docs = len(featuresets)
    label_weights = [(label_counts[lab] / num_docs) for lab in labels]
    print('\nLabel Counts', label_counts)
    #print('Label weights', label_weights)
    # print macro average over all labels
    print('Micro Average Precision\tRecall\t\tF1 \tOver All Labels')
    precision = sum([a * b for a,b in zip(precision_list, label_weights)])
    recall = sum([a * b for a,b in zip(recall_list, label_weights)])
    F1 = sum([a * b for a,b in zip(F1_list, label_weights)])
    print( '\t', "{:10.3f}".format(precision), \
      "{:10.3f}".format(recall), "{:10.3f}".format(F1))

# Function to compute precision, recall and F1 for each label
#  and for any number of labels
# Input: list of gold labels, list of predicted labels (in same order)
# Output: returns lists of precision, recall and F1 for each label
#      (for computing averages across folds and labels)
def eval_measures(gold, predicted, labels):
    
    # these lists have values for each label 
    recall_list = []
    precision_list = []
    F1_list = []

    for lab in labels:
        # for each label, compare gold and predicted lists and compute values
        TP = FP = FN = TN = 0
        for i, val in enumerate(gold):
            if val == lab and predicted[i] == lab:  TP += 1
            if val == lab and predicted[i] != lab:  FN += 1
            if val != lab and predicted[i] == lab:  FP += 1
            if val != lab and predicted[i] != lab:  TN += 1
        # use these to compute recall, precision, F1
        # for small numbers, guard against dividing by zero in computing measures
        if (TP == 0) or (FP == 0) or (FN == 0):
          recall_list.append (0)
          precision_list.append (0)
          F1_list.append(0)
        else:
          recall = TP / (TP + FP)
          precision = TP / (TP + FN)
          recall_list.append(recall)
          precision_list.append(precision)
          F1_list.append( 2 * (recall * precision) / (recall + precision))

    # the evaluation measures in a table with one row per label
    return (precision_list, recall_list, F1_list)

#######################
##  create test file ##
#######################

def create_test_submission(featuresets,test_featuresets,fileName):
  print("---------------------------------------------------")
  print("Training and testing a classifier ")
  test_set = test_featuresets
  training_set = featuresets
  classifier = nltk.NaiveBayesClassifier.train(training_set)

  fw = open(fileName,"w")
  fw.write("PhraseId"+','+"Sentiment"+'\n')
  for test,id in test_featuresets:
    fw.write(str(id)+','+str(classifier.classify(test))+'\n')
  fw.close()

###################
## Main Function ##
###################

# function to read kaggle training file, train and test a classifier 
def processkaggle(dirPath,limitStr):
  # convert the limit argument from a string to an int
  limit = int(limitStr)
  
  os.chdir(dirPath)
  
  f = open('./train.tsv', 'r')
  # loop over lines in the file and use the first limit of them
  phrasedata = []
  for line in f:
    # ignore the first line starting with Phrase and read all lines
    if (not line.startswith('Phrase')):
      # remove final end of line character
      line = line.strip()
      # each line has 4 items separated by tabs
      # ignore the phrase and sentence ids, and keep the phrase and sentiment
      phrasedata.append(line.split('\t')[2:4])
  
  # pick a random sample of length limit because of phrase overlapping sequences
  random.Random(420).shuffle(phrasedata)
  phraselist = phrasedata[:limit]

  print('Read', len(phrasedata), 'phrases, using', len(phraselist), 'random phrases')
  
  print('Phrase list phrases: ')
  for phrase in phraselist[:5]:
    print (phrase)
  
  # create list of phrase documents as (list of words, label)
  phrasedocs_unprocessed = []
  phrasedocs = []

  # add all the phrases
  for phrase in phraselist:
    
    review = int(phrase[1])
    
    ## base version
    tokens = nltk.word_tokenize(phrase[0])
    if (review == 0 or review == 1):
      phrasedocs_unprocessed.append((tokens, 'positive'))
    if (review == 2):
      phrasedocs_unprocessed.append((tokens, 'neutral'))
    if (review == 3 or review == 4):
      phrasedocs_unprocessed.append((tokens, 'negative'))
    

    ## with preprocessing
    #tokenizer = RegexpTokenizer(r'\w+')
    #phrase[0] = preprocessing(phrase[0])
    #tokens = tokenizer.tokenize(phrase[0])
    #phrasedocs.append((tokens, int(phrase[1])))

    ## preprocessing 2
    tokenizer = WordPunctTokenizer()
    phrase[0] = preprocessing(phrase[0])
    tokens = tokenizer.tokenize(phrase[0])
    if (review == 0 or review == 1):
      phrasedocs.append((tokens, 'positive'))
    if (review == 2):
      phrasedocs.append((tokens, 'neutral'))
    if (review == 3 or review == 4):
      phrasedocs.append((tokens, 'negative'))  


  # possibly filter tokens
  
  ## base version
  tokens_unprocessed = get_words(phrasedocs_unprocessed)
  ## preprocessed version
  tokens = get_words_3(phrasedocs)

  # continue as usual to get all words and create word features

  word_features_unprocessed = get_word_features(tokens_unprocessed)
  word_features = get_word_features(tokens)

  ## labels and folds for cross validation
  label_list = [c for (d,c) in phrasedocs]
  labels = list(set(label_list))
  num_folds = 5

  # get features sets for a phrase document, including keyword features and category feature

  ## base version
  featuresets_unprocessed = [(document_features(d, word_features_unprocessed), c) for (d, c) in phrasedocs_unprocessed]
  print('Baseline accuracy: ')
  calculate_accuracy(featuresets_unprocessed)
  cross_validation_PRF(num_folds, featuresets_unprocessed, labels)

  ## preprocessed version
  featuresets = [(document_features(d, word_features), c) for (d, c) in phrasedocs]
  print('Preprocessed accuracy: ')
  calculate_accuracy(featuresets)
  cross_validation_PRF(num_folds, featuresets, labels)

  ##################################################################
  ## Call Featuresets, Calculate Accuracy and Cross Validate Each ##
  ##################################################################

  ## subjectivity featureset
  SL_featuresets = [(SL_features(d, word_features, SL), c) for (d, c) in phrasedocs]
  print('Subjectivity Lexicon accuracy:')
  calculate_accuracy(SL_featuresets)
  cross_validation_PRF(num_folds, SL_featuresets, labels)

  ## negators featureset
  NOT_featuresets = [(NOT_features(d, word_features, negationwords), c) for (d, c) in phrasedocs]
  print('Negation word accuracy:')
  calculate_accuracy(NOT_featuresets)
  cross_validation_PRF(num_folds, NOT_featuresets, labels)

  ## bigram featureset
  bigram_features = get_bigram_features(tokens)
  bigram_featuresets = [(bigram_document_features(d, word_features, bigram_features), c) for (d, c) in phrasedocs]
  print('Bigram features accuracy:')
  calculate_accuracy(bigram_featuresets)
  cross_validation_PRF(num_folds, bigram_featuresets, labels)

  ## POS featureset
  pos_featuresets = [(POS_features(d, word_features), c) for (d, c) in phrasedocs]
  print('POS features accuracy:')
  calculate_accuracy(pos_featuresets)
  cross_validation_PRF(num_folds, pos_featuresets, labels)

  ## LIWC featureset
  liwc_featuresets = [(LIWC_features(d, word_features, poslist, neglist), c) for (d, c) in phrasedocs]
  print('LIWC features accuracy:')
  calculate_accuracy(liwc_featuresets)
  cross_validation_PRF(num_folds, liwc_featuresets, labels)

  ## Hybrid subjectivity and LIWC featureset
  hybrid_featuresets = [(hybrid_features(d, word_features, SL, poslist, neglist), c) for (d, c) in phrasedocs]
  print('hybrid sentiment features accuracy:')
  calculate_accuracy(hybrid_featuresets)
  cross_validation_PRF(num_folds, hybrid_featuresets, labels)

###############################
## Test the actual test data ##
###############################

  f = open('./test.tsv', 'r')
  # loop over lines in the file and use the first limit of them
  testphrasedata = []
  for line in f:
    # ignore the first line starting with Phrase and read all lines
    if (not line.startswith('Phrase')):
      # remove final end of line character
      line = line.strip()
      # each line has 4 items separated by tabs
      # ignore the phrase and sentence ids, and keep the phrase and sentiment
      tempList = []
      if len(line.split('\t')) == 3:
        tempList.append(line.split('\t')[0])
        tempList.append(line.split('\t')[2])
        testphrasedata.append(tempList)
      else :
        tempList.append(line.split('\t')[0])
        tempList.append("")
        testphrasedata.append(tempList)
  
  phraselist = testphrasedata
  print('Read', len(testphrasedata), 'phrases, using', len(phraselist), 'test phrases')
  #for phrase in phraselist[:10]:
    #print (phrase)
  # create list of phrase documents as (list of words)
  phrasedocs = []
  # add all the phrases
  for id,phrase in phraselist:
    # with pre processing
    tokenizer = RegexpTokenizer(r'\w+')
    phrase = preprocessing(phrase)
    tokens = tokenizer.tokenize(phrase)
    phrasedocs.append((id,tokens))
  preprocessedTestTokens = get_test_words(phrasedocs)
  test_word_features = get_word_features(preprocessedTestTokens)
  
  test_featuresets = [(hybrid_features(d, test_word_features, SL, poslist, neglist), id) for (id, d) in phrasedocs]
  create_test_submission(featuresets,test_featuresets,"test_1.csv")


"""
commandline interface takes a directory name with kaggle subdirectory for train.tsv
   and a limit to the number of kaggle phrases to use
It then processes the files and trains a kaggle movie review sentiment classifier.

"""
if __name__ == '__main__':
    if (len(sys.argv) != 3):
        print ('usage: classifyKaggle.py <corpus-dir> <limit>')
        sys.exit(0)
    processkaggle(sys.argv[1], sys.argv[2])