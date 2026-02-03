document.addEventListener("DOMContentLoaded", function () {
  var imageURLs = [
    "images/2025lab.png",
    "images/cornkey3.png",
    "images/heart.png"
  ];

  var randomIndex = Math.floor(Math.random() * imageURLs.length);
  var img = document.createElement("img");
  img.src = imageURLs[randomIndex];
  img.alt = "Ross-Ibarra Lab image";
  img.width = 400;
  img.decoding = "async";
  img.loading = "eager";

  var container = document.getElementById("hero-image");
  if (container) {
    container.appendChild(img);
  }
});
