document.addEventListener("DOMContentLoaded", function () {
// Bio modal
var overlay = document.createElement("div");
overlay.className = "bio-modal-overlay";
overlay.innerHTML =
  '<div class="bio-modal"><button class="bio-modal-close" aria-label="Close">&times;</button><h3 class="bio-modal-name"></h3><p class="bio-modal-text"></p></div>';
document.body.appendChild(overlay);

var modalName = overlay.querySelector(".bio-modal-name");
var modalText = overlay.querySelector(".bio-modal-text");

document.addEventListener("click", function (e) {
  var target = e.target.nodeType === 3 ? e.target.parentElement : e.target;
  var bioBtn = target.closest(".bio-btn");
  var closeBtn = target.closest(".bio-modal-close");

  if (bioBtn) {
    e.preventDefault();
    modalName.textContent = bioBtn.dataset.name || "";
    modalText.innerHTML = bioBtn.dataset.bio || "";
    overlay.classList.add("active");
    return;
  }

  if (target === overlay || closeBtn) {
    overlay.classList.remove("active");
  }
});

document.addEventListener("keydown", function (e) {
  if (e.key === "Escape") overlay.classList.remove("active");
});
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
