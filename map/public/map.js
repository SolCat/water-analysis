const width = 800, height = 800;
const colors = ["#2ecc71", "#3498db", "#e67e22", "#f1c40f", "#9b59b6"]
const path = d3.geoPath();
const projection = d3.geoConicConformal()
  .center([2.5, 46.8])
  .scale(4000)
  .translate([width/2, height/2]);
path.projection(projection);
const svg = d3.select('#map').append("svg")
    .attr("id", "svg")
    .attr("width", width)
    .attr("height", height);
const france = svg.append("g");
const cities = svg.append("g");

$( document ).ready(function() {
  d3.json('custom.geo.json').then(function(geojson) {
      france.selectAll("path")
          .data(geojson.features)
          .enter()
          .append("path")
          .attr("d", path);
  });

  d3.json('data.json').then(function(classif) {
      var data = new Array();
      for (var i in classif){
          data.push( classif[i] );
      }
      console.log(data);
      var circles = cities.selectAll("circle")
          .data(data)
          .enter()
          .append("circle")
          .attr("cx", function (d) { return projection([d.pos.lng,d.pos.lat])[0]; console.log("oui"); })
          .attr("cy", function (d) { return projection([d.pos.lng,d.pos.lat])[1]; })
          .attr("r", "6")
          .attr("fill", function(d){ return colors[d.k-1] });
      console.log(circles);
  });
});
