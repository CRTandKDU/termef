const jsdom = require('jsdom');
const { JSDOM } = jsdom;

const d3 = import('d3');

var epc = require("elrpc");

const renderD3ImageFile = async (data) => {
    // DOM emulation
    const dom = new JSDOM('<!DOCTYPE html><body><svg id="container"></svg></body></html>',
			 { runScripts: "dangerously" })
    // get the SVG container element
    const svgElement = dom.window.document.getElementById("container");

    // d3
    // select the SVG element using D3.js
    const svg = d3.select(svgElement)
    // not sure if this is necessary
	  .attr('xmlns', "http://www.w3.org/2000/svg");

    const svgWidth = 1080;
    const svgHeight = 300;

    /* Sizing the svg image
       Since our DOM is emulated, we cannot enquire any sizing information from it.
       We can either size the SVG by (1) providing fixed height/width values or (2) by using the viewBox attribute.

       The following code uses the viewBox attributes
    */
    svg.attr('width', `${svgWidth}px`)
	.attr('height', `${svgHeight}px`);

    // any kind of D3.js magic

    const x = d3.scaleLinear([0, 100], [0, svgWidth]);
    const coreGroup = svg.append('g').attr('class', 'core-group');
    const barHeight = 50;
    const barTopMargin = 10;
    const someRandomBars = coreGroup.selectAll('g.bar')
	  .data([10, 75, 100, 24, 55])
	  .enter()
	  .append('g')
	  .attr('class', 'bar')
	  .attr('transform', (d,i) => `translate(0 ${i * (barHeight + barTopMargin)})`);
    someRandomBars.append('rect')
	.attr('x', 0).attr('y', 0)
	.attr('width', (d) => {
	    return x(d) + 'px';
	})
	.attr('height', `${barHeight}px`)
    // all styling has to be done inline
	.attr('fill', 'blue');
};
    
epc.startServer().then(function(server) {
	server.defineMethod("echo", function(args) {
		return args;
	});
	server.wait();
});

