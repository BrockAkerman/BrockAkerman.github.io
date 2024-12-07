const aboutMeButton = document.getElementById('aboutMeButton');
const lightbox = document.getElementById('lightbox');
const closeLightbox = document.getElementById('closeLightbox');

aboutMeButton.addEventListener('click', () => {
    lightbox.classList.remove('hidden');
});

closeLightbox.addEventListener('click', () => {
    lightbox.classList.add('hidden');
});
