tbibloom
========

POC Bloom filter for mulit-term query of a term based index on Riak
Project Skeleton for the tbibloom app.

You should find in this directory:

README : this file
Makefile : simple make commands
rebar : the Rebar build tool for Erlang applications
rebar.config : configuration for Rebar
start.sh : simple startup script for running tbibloom
/ebin
  /tbibloom.app : the Erlang app specification
/src
  /tbibloom_app.erl : base module for the Erlang application
  /tbibloom_sup.erl : OTP supervisor for the application
  /tbibloom_resource.erl : a simple example Webmachine resource
/priv
  /dispatch.conf : the Webmachine URL-dispatching table
  /www : a convenient place to put your static web content

You probably want to do one of a couple of things at this point:

0. Build the skeleton application:
   $ make
   - or -
   $ ./rebar compile

1. Start up the skeleton application:
   $ ./start.sh

2. Change the basic application:
   edit src/tbibloom_resource.erl

3. Add some new resources:
   edit src/YOUR_NEW_RESOURCE.erl
   edit priv/dispatch.conf
