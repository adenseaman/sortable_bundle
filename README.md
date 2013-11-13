# Sortable Bundle

## What is this?

This is my solution to the <a href="http://sortable.com/blog/coding-challenge/">Sortable Coding Challenge</a>.

It is written in <a href="http://www.erlang.org">Erlang</a>, because at the time of this writing nobody has submitted a solution to the challenge in Erlang yet, and I really like it.


One of Erlang's strong points is the ability to do distributed and fault-tolerant computation.  Simply using Erlang in a serial fashion to solve the Sortable challenge wasn't a very interesting prospect to me, so I wrote a generic application that is able to map a function to data in a parallel and fault-tolerant way, and then applied it to solving the Sortable challenge.

## Directory Structure

This repository consists of two directories:

* distributed\_map :  this is the parallel and fault-tolerant map application
* sortable\_challenge : this is the actual Sortable challenge program


These are two separate programs that can run on their own, but in order to do the sortable\_challenge in parallel, it's necessary to have the distributed\_map application.

# Brief Instructions

1. git clone https://github.com/bisrcacuo/sortable_bundle
2. sudo apt-get install erlang
3. copy Sortable Challenge data files, *listings.txt* and *products.txt*, into *sortable\_bundle/sortable\_challenge/data* directory
4. cd sortable\_bundle; ./build.sh
5. cd sortable\_challenge; ./runme.distributed\_map.sh
6. follow instructions and type **q().** when it's finished
7. look for results in *data/results.distributed_map.txt*

# Detailed Instructions

Once you've cloned this repository, you need to do a few things:

1. Install Erlang on your computer.  If you're running Debian or Ubuntu, it's a simple matter of **apt-get install erlang**.  This will install a bunch of things, among which is the Erlang build system needed to build the applications.
2. run **./build.sh** in the *sortable\_bundle* directory.  This will compile the _\*.erl_ files in the *src* directories into _\*.beam_ files in the *ebin* directories, for both the sortable\_challenge and distributed\_map applications.
3. put your *listings.txt* and *products.txt* source JSON data files into the *sortable\_challenge/data* directory.  There are two empty placeholder files there now to make Git happy.

## Running the Applications

### distributed\_map

First enter the *distributed\_map* directory and execute the **./runme.sh** file from the console.  This will fire up several Erlang nodes on your computer, as well as a console node, connect them all together in a cluster, and give you some instructions.

Type **distmap:slow\_example().** to see a play-by-play of the distributed processing of a trivial job.  This will prompt you for input and intentionally runs very slowly so you can see what's going on.  The most interesting part is when it kills one of the Erlang nodes and later brings it back, all without interrupting the job processing.

If you type **distmap:fast\_example().** you'll see a play-by-play of processing a trivial but much larger job at full speed, without killing an Erlang node.

Remember to type **q().** in the Erlang console when you're finished in order to exit.

Although this example runs on only one computer, the distributed\_map application can run on many multicore/multiprocessor computers connected together through a network.

### sortable\_challenge

After you get an idea of how the distributed\_map program works, it's time to get down to solving the Sortable challenge.

Enter the *sortable\_challenge* directory and type **./runme.stand\_alone.sh** to first solve the Challenge without parallel processing.  Make sure you've put your *listings.txt* and *products.txt* files in the *data* directory, otherwise nothing interesting will happen.  This doesn't require any user input other than typing **q().** to exit the Erlang shell.

This creates a JSON file *results.stand\_alone.txt* in the *data* directory.


To use the distributed\_map application to solve the challenge, exit Erlang and type **./runme.distributed\_map.sh**.

You'll need to type **challenge:distributed\_map().** to get things started, and it will prompt you for the client coordinator to which you'd like to submit the data.

You'll get a play-by-play of what's going on as the data is crunched, then type **q().** to exit the Erlang shell.

This will create a JSON file *results.distributed\_map.txt* in the *data* directory.


Both of the result files should be exactly the same, and are my solution to the Sortable Challenge.

# Copyright

With the exception of the file *mochijson2.erl* in the *sortable\_challenge/src* directory, which is part of the <a href="https://github.com/mochi/mochiweb">MochiWeb project</a>, all of the source code was written by, and is copyrighted by, Aden Seaman.  No part of his source code may be used without permission.
