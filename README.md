DataPipe
========
DataPipe is a data processing tool that can work with larger-than-memory files.

![Screenshot of DataPipe](https://raw.githubusercontent.com/fdb/datapipe/master/screenshot.png)

## Downloading

DataPipe requires that you have [Java](https://java.com/en/download/) installed.

Download the latest version from the [releases page](https://github.com/fdb/datapipe/releases). It looks like `datapipe-0.x.x-standalone.jar`.

## Using

DataPipe always requires an *input file* and an *output file*. The input file is typically a large file that needs to be processed. DataPipe will read in this file. The output file is a new file where DataPipe can write the data to.

Just below the input and output file, you can type a list of commands. Commands will be executed as operations on the original file. The final output will be sent to the output file.

At the bottom, a *preview* of the file is displayed. It shows only the first 100 rows of the output file.

## To build

    lein uberjar

## License

Copyright © 2015 Frederik De Bleser

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
