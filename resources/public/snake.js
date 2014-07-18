var $canvas;
var $score;
var sz = 10;
var ws;
var snake = [];
var apple = null;


var dirMap = {
  "38": "up",
  "40": "down",
  "37": "left",
  "39": "right"
};

var points = {};

function addPoint(id) {
  points[id] = points[id] || 0;
  points[id] += 1;
}

var ID = null;
var $canvas;

function turn(dir) {
  return JSON.stringify({"turn": dir});
}

function init() {
  $canvas = document.createElement("canvas");
  document.body.appendChild($canvas);
  $canvas.style.border = "solid 1px";

  $score = document.createElement("div");
  document.body.appendChild($score);
  $canvas.width = 40 * sz;
  $canvas.height = 40 * sz;
  ws = new WebSocket("ws://" + document.location.hostname  + ":8080/ws");
  ws.onmessage = function(e) {
    //debugger;
    var result = JSON.parse(e.data);
    if (result.hasOwnProperty("grid")) {
      update(result.grid);
    }
    if (result.hasOwnProperty("id")) {
      ID = result.id;
    }
  }

  window.onkeydown = function(e) {
    var dir = dirMap[e.keyCode];
    if (dir) {
      ws.send(turn(dir));
    }
  }
}



function update(result) {
  points = {};
  $score.textContent = "";
  var ctx = $canvas.getContext("2d");
  ctx.strokeRect(0, 0, $canvas.width, $canvas.height);
  ctx.clearRect(0, 0, 800, 600);
  var color;
  for (var i = 0; i < result.length; i += 1) {
    var line = result[i];
    for (var j = 0; j < line.length; j += 1) {
      if (line[j] != null) {
        if (line[j] == "apple") {
          color = "red";
        } else if (line[j] == ID) {
          color = "green";
        } else {
          color = "black";
        }
        addPoint(line[j]);
        ctx.fillStyle = color;
        ctx.fillRect(i * 10, j * 10, 10, 10);
      }
    }
  }
  for (var id in points) {
    if (points.hasOwnProperty(id)) {
      var player;
      if (id == ID) {
        player = "ME!";
      } else {
        player = "player " + id;
      }
      if (id != "apple") {
        $score.innerHTML += "<br />" + player + " " + points[id];
      }
    }
  }
}

window.onload = init;
