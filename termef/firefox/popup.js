// Firefox variant
let termefselection;

// Instantiates <ul/> into a jstree and bind events
const initfill = () => {
    jQuery('#jstree_demo_div')
	.jstree()
	.bind("dblclick.jstree", function (evt) {
	    let tree = jQuery(this).jstree();
	    let node = tree.get_node(evt.target);
 	    // var nodePath = tree.get_path(node).join("/");
	    let href = node['li_attr']['href'];
	    console.log(href);
	    // Do some action
	    if( undefined != href ){
		browser.tabs.create({ url: href });
	    }
	});
};

// Build a breadcrumbs-like list of ancestors in the ontology
const ancestors = ( arr ) => {
    let str = "";
    for( elt in arr ){
	str += arr[elt]['name'];
	str += " > ";
    }
    return str.slice(0,-3);
};

// Format the TerMef results as an HTML nested list for `jsTree'
const infobox = ( data ) => {
    let node;
    let resp = '<ul>';
    let groups = {};
    
    // First pass: map data to html unnumbered lists
    for( node in data['items'] ){
	let vroot = data['items'][node]['attributes'].find( x => x['attribute'] == "Groupe" );
	let v     = vroot;
	let url   = data['items'][node]['url'];
	if( undefined != v){
	    // Filter with selection or don't if selection is empty or undefined
	    if( undefined == termefselection ||
		0 == termefselection.length  ||
		termefselection.includes( v['values'][0][0]['name'] ) )
	    {
		// Path in ontology 
		resp += "<li>" + ancestors( v['values'][0] ) + '<ul>';
		let termresp = "";
		
		// Subitem `Term'
		v = data['items'][node]['attributes'].find( x => x['attribute'] == "Nom" );
		if( undefined != v ){
		    termresp += "<li href=\"" + url + "\">Terme : <b>" + v['values'][0] + "</b></li>";
		}

		// Subitem `Definition'
		v = data['items'][node]['attributes'].find( x => x['attribute'].includes("finition") );
		if( undefined != v ){
		    termresp += "<li>D\xE9finition : " + v['values'][0] +'</li>';
		}
		
		// Subitem `Notes', as a subtree
		v = data['items'][node]['attributes'].find( x => x['attribute'] == "Note" );
		if( undefined != v ){
		    termresp += "<li>Notes<ul>";
		    for( subnode in v['values'] ){
			termresp += "<li>" + v['values'][subnode] + "</li>";
		    }
		    termresp += '</ul></li>';
		}
		
		// Subitem `Publication date'
		v = data['items'][node]['attributes'].find( x => x['attribute'] == "Date de parution au JO" );
		if( undefined != v ){
		    termresp += "<li>JO du " + v['values'][0] + ".</li>";
		}

		let idx = ancestors( vroot['values'][0] );
		if( undefined == groups[ idx ] ){
		    groups[ idx ] = new Array( termresp );
		}
		else{
		    groups[ idx ].push(termresp);
		}
		resp += termresp + '</ul></li>';
	    }
	}
    }
    resp += '</ul>';

    // Second pass: reduce by ancestor
    resp = '<ul>';
    for (const [key, value] of Object.entries(groups)) {
	resp += '<li>' + key + '<ul>';
	for( idx in value ){
	    resp += value[idx];
	}
	resp += '</ul></li>';
    }
    resp += '</ul>';
    jQuery('#jstree_demo_div').jstree('destroy');
    jQuery('#jstree_demo_div').empty();
    jQuery('#jstree_demo_div').append(resp);
    jQuery( initfill() );

    return resp;
};



browser.storage.local.get( 'termefselection', (data) => {
    // Set global
    termefselection = data.termefselection;
    console.log( data.termefselection );
    if( undefined == termefselection ||	0 == termefselection.length ){
	jQuery( "#options" ).text( "R\xE9f\xE9rentiels : tous" );
    }
    else{
	jQuery( "#options" ).text( "R\xE9f\xE9rentiels : " + termefselection.join() );	
    }
});

// Attach `enter' key to query execution in the input field
jQuery( '#qtext' ).on( 'keypress', function(e) {
    if(e.which == 13) {
        $('#query').trigger('click');
    }
});

// When the button is clicked, get the term infobox
jQuery('#query').click( async () => {
    let [tab] = await browser.tabs.query({ active: true, currentWindow: true });
    let color = jQuery('#qtext').val();
    let query = '?q=' + color + '&s=1&mode=exact&field=label';
    jQuery.ajax( "https://terminologie.finances.gouv.fr/search/infobox" + query,
		 {
		     success: (data, textStatus, jqXHR ) => {
			 let resp = infobox( data );
			 // jQuery('#infobox').val( resp );
		     },
		     error: ( jqXHR, textStatus, errorThrown ) => {
			 // jQuery('#infobox').val( "TerMef inaccessible!" );
		     }
		 });
});
