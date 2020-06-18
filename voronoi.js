// !preview r2d3 d3_version = 4

// Based on: https://bl.ocks.org/mbostock/4060366
// Rendering
svg.on("touchmove mousemove", moved);

//var sites = d3.range(100)
//    .map(function(d) { return [Math.random() * width, Math.random() * height]; });

var  margin = { top: 10, right: 30, bottom: 50, left: 0 };
    var w = width;
    var h = height;
    var padding = 0;
    var border=1;
    var bordercolor='black';

var borderPath = svg.append('rect')
      .attr("height", h)
  .attr("width", w)
  .style("stroke", bordercolor)
  .style("fill", "none")
  .style("stroke-width", border);
      
  
var x = d3.scaleLinear()
.domain([options.x_min,options.x_max])
.range([60, width- 60]);

var y = d3.scaleLinear()
.domain([options.y_min,options.y_max])
.range([height - 30, 30]);

var sites = [];
for (i=0;i<Object.keys(data).length;i++){
// sites[i] = [(data[String(i)].x + Math.abs(options.x_min),(data[String(i)].y + Math.abs(options.y_min))]
sites[i] = [x(data[String(i)].x),y(data[String(i)].y)];
}

var voronoi = d3.voronoi()
  .extent([[-20, -20], [width + 20, height + 20]]);
  
svg.selectAll('g,rect').remove();
var polygon = svg.append("g")
  .attr("class", "polygons")
  .selectAll("path")
  .data(voronoi.polygons(sites))
  .enter().append("path")
// .style("fill", function(d) {return "green"})
// .on("mouseover", showTooltip)
// .on("mouseout",  removeTooltip)

.on("mouseover", function(d,i){
  //console.log("Tesselation Highlighted", this);
  d3.select(this)
        //.transition()
        //.duration(100)
        //.attr('r', 20)
        //.attr('fill', '#ff0000')
        .style('opacity', 0.25);
  //d3.select(this).style('opacity',0.25);
})

.on("mouseout", function(d,i){
  //console.log("No Tesselation Highlighted", this);
  d3.select(this)
        //.transition()
        //.duration(100)
        //.attr('r', 10)
        //.attr('fill', '#000000')
        .style('opacity', 1);
  //d3.select(this).style('opacity', 1);
})




  .call(redrawPolygon);

var format = d3.format(".2f")

polygon.append("title")
    .text(function(d,i) { return "Quant.Error = " + format(data[String(i)].grad_values) });

var link = svg.append("g")
    .attr("class", "links")
  .selectAll("line")
  .data(voronoi.links(sites))
  .enter().append("line")
    .call(redrawLink);

var site = svg.append("g")
    .attr("class", "sites")
  .selectAll("circle")
  .data(sites)
  .enter().append("circle")
    .attr("r", 2.5)
    .call(redrawSite);

function moved() {
//  sites[0] = d3.mouse(this);
//  redraw();
}

function redraw() {
  var diagram = voronoi(sites);
  polygon = polygon.data(diagram.polygons()).call(redrawPolygon);
  link = link.data(diagram.links()), link.exit().remove();
  link = link.enter().append("line").merge(link).call(redrawLink);
  site = site.data(sites).call(redrawSite);
}

// https://stackoverflow.com/questions/43419351/generated-color-of-voronoi-polygon-in-d3-js
// document.querySelectorAll("path").forEach(polygon => polygon.style.fill = getRandomColor());

function redrawPolygon(polygon) {
  var color = d3.interpolateCool
  
  // if(options.chart_type == "Voronoi Cells"){
    polygon
      .attr("d", function(d) { return d ? "M" + d.join("L") + "Z" : null; })
      .style("fill", function(d,i) { return color((data[String(i)].grad_values - options.grad_min)/(options.grad_max - options.grad_min))});
  // }
}

function redrawLink(link) {
  // console.log(options.chart_type)
  if(options.chart_type == true){
    link
      .attr("x1", function(d) { return d.source[0]; })
      .attr("y1", function(d) { return d.source[1]; })
      .attr("x2", function(d) { return d.target[0]; })
      .attr("y2", function(d) { return d.target[1]; });
  } else {
    // link.exit().remove();
    link
      .attr("x1", function(d) { return null; })
      .attr("y1", function(d) { return null; })
      .attr("x2", function(d) { return null; })
      .attr("y2", function(d) { return null; });
  }
}

function redrawSite(site) {
  site
      .attr("cx", function(d) { return d[0]; })
      .attr("cy", function(d) { return d[1]; });
}

//http://bl.ocks.org/nbremer/65f03d1ebd1742196200
//Show the tooltip on the hovered over circle
function showTooltip(d) {
  $(this).popover({
    placement: 'auto top', //place the tooltip above the item
    container: '#chart', //the name (class or id) of the container
    trigger: 'manual',
    html : true,
    content: function() { //the html content to show inside the tooltip
      return "<span style='font-size: 11px; text-align: center;'>" + "sds" + "</span>"; }
  });
  $(this).popover('show');      
}//function showTooltip

//Hide the tooltip when the mouse moves away
function removeTooltip() {
  //Hide the tooltip
  $('.popover').each(function() {
    $(this).remove();
  });   
}//function removeTooltip
