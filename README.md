UMass BioNLP Event Extractor
============================

# Installation

Get maven, untar/unzip and then run

    $ mvn compile

Get mongo, and run the mongo server

    $ mongod

You should also have an installation of the BioNLP reranking parser by David McClosky on your machine.

# Preprocessing

Before we train, we need to go through two preprocessing steps that prepare the data. First call

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx1g -Dprop=props/example.prop  -cp %classpath cc.refectorie.proj.bionlp2011.LowLevelAnnotation dev train test"

This will add tokenize, sentence-split etc. the data specified in the prop file.

Next we run

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx1g -Dprop=props/example.prop  -cp %classpath cc.refectorie.proj.bionlp2011.App dev train test"

This will prepare some candidate structures that are used during inference/learning.

# Learning

Finally, you're ready to train the model

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx80g -Dprop=props/example.prop -cp %classpath cc.refectorie.proj.bionlp2011.BioNLPLearner"

This will store weights for different epochs into $UMASSDIR/weights/[epoch]

Learning also runs evaluation on test and development sets. The results will appear in the outDir
specified in the prop file.

# Testing

You can use the stored weights in a standalone tool that applies the complete preprocessing chain and the event
extractor model to input files. For this first set weightsSrc=weights/[epoch of choice] in the prop file.
Then run the standalone tool as follows:

    $ mvn exec:exec -Dexec.executable="java" -Dexec.args="-Xmx80g -Dprop=props/example.prop -cp %classpath cc.refectorie.proj.bionlp2011.UMassBioEventExtractor [txt file] [a1file] [destfile]"





