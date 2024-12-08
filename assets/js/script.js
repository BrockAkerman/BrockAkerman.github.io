// Get references to the "About Me" button, lightbox, and close button
const aboutMeButton = document.getElementById('aboutMeButton');
const lightbox = document.getElementById('lightbox');
const closeLightbox = document.getElementById('closeLightbox');

// Show the lightbox when "About Me" button is clicked
aboutMeButton.addEventListener('click', () => {
    lightbox.classList.remove('hidden');  // Show the lightbox
});

// Hide the lightbox when the "Close" button is clicked
closeLightbox.addEventListener('click', () => {
    lightbox.classList.add('hidden');  // Hide the lightbox
});



// Get elements
const menuToggle = document.getElementById('menuToggle');
const navMenu = document.getElementById('navMenu');

// Toggle menu visibility on click
menuToggle.addEventListener('click', () => {
    navMenu.classList.toggle('active');  // Toggle the active class to show or hide the menu
});
