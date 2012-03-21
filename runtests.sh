#!/bin/sh
./sbt test
./sbt "run tests/typeable.abercone tests/pairsUsingPartialFunctions.abercone tests/test1.abercone tests/closuretests.abercone"
