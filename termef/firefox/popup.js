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
    for( node in data['items'] ){
	let v	= data['items'][node]['attributes'].find( x => x['attribute'] == "groupe" );
	let url = data['items'][node]['url'];
	if( undefined != v){
	    if( undefined == termefselection ||
		0 == termefselection.length  ||
		termefselection.includes( v['values'][0][0]['name'] ) )
	    {
		// Path in ontology 
	    resp += "<li>" + ancestors( v['values'][0] ) + '<ul>';
	    
	    // Subitem `Term'
	    v = data['items'][node]['attributes'].find( x => x['attribute'] == "Nom" );
	    if( undefined != v ){
		resp += "<li href=\"" + url + "\">Terme : <b>" + v['values'][0] + "</b></li>";
	    }

	    // Subitem `Definition'
	    v = data['items'][node]['attributes'].find( x => x['attribute'] == "d\xE9finition" );
	    if( undefined != v ){
		resp += "<li>D\xE9finition : " + v['values'][0] +'</li>';
	    }
	    
	    // Subitem `Notes', as a subtree
	    v = data['items'][node]['attributes'].find( x => x['attribute'] == "note" );
	    if( undefined != v ){
		resp += "<li>Notes<ul>";
		for( subnode in v['values'] ){
		    resp += "<li>" + v['values'][subnode] + "</li>";
		}
		resp += '</ul></li>';
	    }
	    
	    // Subitem `Publication date'
	    v = data['items'][node]['attributes'].find( x => x['attribute'] == "date de parution au JO" );
	    if( undefined != v ){
		resp += "<li>JO du " + v['values'][0] + ".</li>";
	    }
		resp += '</ul></li>';
	    }
	}
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
