// pull in desired CSS/SASS files
// require( './main.scss' );
// var $ = jQuery = require( '../../node_modules/jquery/dist/jquery.js' );           // <--- remove if jQuery not needed
// require( '../../node_modules/bootstrap-sass/assets/javascripts/bootstrap.js' );   // <--- remove if Bootstrap's JS not needed

// inject bundled Elm app into div#main
var Elm = require( 'src/Editor.elm' );
Elm.Editor.embed( document.getElementById( 'main' ) );

