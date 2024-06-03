#!/usr/bin/env -S node
/*=====================================================================*/
/*    serrano/prgm/project/bbv-tests/bigloo/addusages.mjs              */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Thu May 30 09:27:20 2024                          */
/*    Last change :  Thu May 30 09:42:39 2024 (serrano)                */
/*    Copyright   :  2024 Manuel Serrano                               */
/*    -------------------------------------------------------------    */
/*    Merge a "static CFG json dump" and a "dynamic profiling count".  */
/*=====================================================================*/

// example:
// BIGLOOBBVDUMPJSON=true BIGLOOBBVVLENGTH=true BIGLOOBBVVERSIONLIMIT=4 bigloo -srfi arithmeticG -w -O3 -saw -fsaw-bbv bbv.bgl../recursive/fib.scm 
// BIGLOOSAWPROFILE=true BIGLOOBBVVLENGTH=true BIGLOOBBVVERSIONLIMIT=4 bigloo -srfi arithmeticG -w -O3 -saw -fsaw-bbv bbv.bgl ../recursive/fib.scm
// ./a.out > fib.prof.json
// addcount.mjs bbv-fib~0.bbv.json fib.prof.json

/*---------------------------------------------------------------------*/
/*    Module                                                           */
/*---------------------------------------------------------------------*/
import * as fs from "fs";

/*---------------------------------------------------------------------*/
/*    readJson ...                                                     */
/*---------------------------------------------------------------------*/
function readJson(path) {
   try {
      const res = fs.readFileSync(path, { encoding: "utf8" });
      return JSON.parse(res);
   } catch(e) {
      console.error(`gallery: cannot read json file "${path}"`);
      throw e;
   }
}

/*---------------------------------------------------------------------*/
/*    main ...                                                         */
/*---------------------------------------------------------------------*/
function main(argv) {
   const cfg = readJson(argv[2]);
   const dyn = readJson(argv[3]);
   const fun = cfg.specializedCFG[0].bbs;
   const bbs = [];

   cfg.specializedCFG.forEach(b => bbs[b.id] = b);

   dyn.forEach(b => {
      if (b?.fun === fun) {
	 const bb = bbs[b.id];

	 if (bb) {
	    bb.usage = b.count;
	 }
      }
   });

   console.log(JSON.stringify(cfg));
}

main(process.argv);
