const express = require('express')
const path = require('path')
const app = express()

app.use(express.static('public'));

app.get('/', function (req, res) {
  res.sendFile(path.join(__dirname, './public', 'map.html'));
})

app.listen(3000, function () {
  console.log('Map waters kmeans classification - localhost:3000')
})
