UCL/UMass BioNLP Event Extractor
============================

# Installation

Get maven, untar/unzip and then run

    $ mvn compile

If it looks like dependencies cannot be found, first try [this](https://groups.google.com/a/factorie.cs.umass.edu/forum/?fromgroups=#!msg/discuss/7F8laYbmU-w/eFJJ_tlerrMJ). 

ucleed stores preprocessed data in a mongo database. Hence you need to get mongo, and run the mongo server

    $ mongod

You should also have an installation of the BioNLP reranking parser by David McClosky on your machine.

You also need to configure a few directory locations. Copy the example in `src/main/resources/props/example.prop` and modify as needed.

## Setting up the syntactic parser

ucleed uses the [reranking parser](https://bitbucket.org/bllip/bllip-parser/src) by David McClosky, in combination with his [Improved self-trained biomedical parsing model](http://nlp.stanford.edu/~mcclosky/biomedical.html). In the configuration file, set `rerankparser` to the main directory of the parser, and `biomodel` to the directory of the biomedical parsing model. 

# Preprocessing

Before we train, we need to go through two preprocessing steps that prepare the data. 

## Data preprocessing

First call

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx1g -Dprop=props/example.prop  -cp %classpath cc.refectorie.proj.bionlp2011.ClearRaw"

to clear the database (this is actually only necessary if you want to rerun experiments but it shouldn't hurt). Then do

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx1g -Dprop=props/example.prop  -cp %classpath cc.refectorie.proj.bionlp2011.LowLevelAnnotation dev train test"

This will add tokenize, sentence-split etc. the data specified in the prop file.

## Feature preprocessing

Next we run

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx1g -Dprop=props/example.prop  -cp %classpath cc.refectorie.proj.bionlp2011.ClearAnnotated"

to initialize learning (again only necessary if you want to retrain with newly preprocessed data). Then do:

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx1g -Dprop=props/example.prop  -cp %classpath cc.refectorie.proj.bionlp2011.App dev train test"

This will prepare some candidate structures that are used during inference/learning.

# Learning

Now copy data with features to the learning KB:

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx1g -Dprop=props/example.prop  -cp %classpath cc.refectorie.proj.bionlp2011.ClearLearningKB"

Finally, you're ready to train the model

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx8g -Dprop=props/example.prop -cp %classpath cc.refectorie.proj.bionlp2011.BioNLPLearner"

This will store weights for different epochs into $UMASSDIR/weights/[epoch]

Learning also runs evaluation on test and development sets. The results will appear in the outDir
specified in the prop file.

# Testing

You can use the stored weights in a standalone tool that applies the complete preprocessing chain and the event
extractor model to input files. For this first set `weightsSrc=weights/[epoch of choice]` in the prop file. Generally epoch 4 or 5 seems to give good results, but can check what works best on the dev set. 

Then run the standalone tool as follows:

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx80g -Dprop=props/example.prop -cp %classpath cc.refectorie.proj.bionlp2011.UMassBioEventExtractor [txt file] [a1file] [destfile]"

# Further Reading and Citations

The most relevant citation for this work is our [EMNLP paper](http://riedelcastro.github.com/publications/details/riedel11fast.html).
Further details can be found in our BioNLP shared task papers on 
[system combination](http://riedelcastro.github.com/publications/details/riedel11model.html) and 
[dual decomposition](http://riedelcastro.github.com/publications/details/riedel11robust.html).



