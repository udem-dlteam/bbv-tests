/*=====================================================================*/
/*    serrano/prgm/project/bbv-tests/tests/paper/js/bague.js           */
/*    -------------------------------------------------------------    */
/*    Author      :  Pierre Weis                                       */
/*    Creation    :  Fri Apr  1 10:00:21 1994                          */
/*    Last change :  Thu Mar 21 19:02:19 2024 (serrano)                */
/*    -------------------------------------------------------------    */
/*    Resolution recursive du Baguenaudier: bench les appels de        */
/*    fonctions et les acces aux vecteurs                              */
/*    avec 21 pierres le nombre de coups est 1398101                   */
/*    avec 24 pierres le nombre de coups est 11184810                  */
/*    f (n+1) = 2*f(n) + n mod 2 avec f 1 = 1                          */
/*=====================================================================*/
"use strict";

let nombre_de_coups = 0;
const  nombre_de_pierres = 28;

const une_pierre = 1;
const une_case_vide = 0;

let jeu = new Array( nombre_de_pierres ).fill(une_pierre);

function init_jeu() {
   nombre_de_coups = 0;
   for (let i = nombre_de_pierres - 1; i >= 0; i--) {
      jeu[i] = une_pierre;
   }
}

function la_case( n ) {
   return n - 1;
}

function enleve_la_pierre( n ) {
   if( jeu[ la_case( n ) ] === une_pierre ) {
      jeu[ la_case( n ) ] = une_case_vide;
   }
}

function pose_la_pierre( n ) {
   if( jeu[ la_case( n ) ] === une_case_vide ) {
      jeu[ la_case( n ) ] = une_pierre;
   }
}

function autorise_mouvement( n ) {
   switch( n ) {
      case 1:
	 return true;
      case 2:
	 return jeu[ la_case( 1 ) ] === une_pierre;
      default:
	 if( jeu[ la_case( n - 1 ) ] !== une_pierre )
	    return false;

	 let b = true;

	 for( let i = 0; i <= la_case( n - 2 ); i++ ) {
	    b = b && (jeu[ i ] === une_case_vide);
	    i++;
	 }
	 
	 return b;
   }
}

function enleve_pierre( n ) {
   nombre_de_coups++;
   if( autorise_mouvement( n ) ) {
      enleve_la_pierre( n );
   }
}

function pose_pierre( n ) {
   nombre_de_coups++;
   if( autorise_mouvement( n ) ) {
      pose_la_pierre( n );
   }
}

function bague( n ) {
   switch( n ) {
      case 1:
	 enleve_pierre( 1 );
	 return;
	 
      case 2:
	 enleve_pierre( 2 );
	 enleve_pierre( 1 );
	 return;
	 
      default:
	 bague( n - 2 );
	 enleve_pierre( n );
	 repose( n - 2 );
	 bague( n - 1 );
   }
}

function repose( n ) {
   switch( n ) {
      case 1:
	 pose_pierre( 1 );
	 return;

      case 2:
	 pose_pierre( 1 );
	 pose_pierre( 2 );
	 return;

      default:
	 repose( n - 1 );
	 bague( n - 2 );
	 pose_pierre( n );
	 repose( n - 2 );
   }
}

/*---------------------------------------------------------------------*/
/*    main ...                                                         */
/*---------------------------------------------------------------------*/
const unknown = eval("(x, y) => x");

function run(o) {
   let n = o?.nombre_de_pierres || unknown(28, 10);
   let res = false;
   
   while( n-- > 0 ) {
      init_jeu();
      bague(nombre_de_pierres);
      res = [nombre_de_pierres, nombre_de_coups];
   }
   
   return res;
}

function check(result) {
   if (!(result[0] === 28 && result[1] === 178956970)) {
      console.log("result=", result);
      throw "test failed";
   }
}

function main(argv) {
   const arg = argv.length > 2 ? argv[2] : "{repeat: 1, nombre_de_pierres: 28}"
   const { repeat, nombre_de_pierres } = JSON.parse(arg);
   const o = { nombre_de_pierres };
   let result = false;

   for (let r = 0; r < repeat; r++) {
      result = run(o);
   }
   
   check(result);
}

main(process.argv);
