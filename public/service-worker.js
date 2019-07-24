const CACHE_NAME = "v0.8.0";

const FILE_URLS = [
	".",
	"manifest.json",
	"index.css",
	"index.js",
	"res/icon_192x192.png",
	"res/icon_256x256.png",
	"res/icon_512x512.png",
	"res/icon_maskable.png",
];

self.addEventListener('install', function (event) {
	event.waitUntil(
		caches.open(CACHE_NAME).then(cache => cache.addAll(FILE_URLS))
	);
});

self.addEventListener('activate', function (event) {
	event.waitUntil(
		caches.keys().then(names => Promise.all(
			names.filter(name => name !== CACHE_NAME).map(name => caches.delete(name))
		))
	);
});

self.addEventListener('fetch', function (event) {
	event.respondWith(
		caches.match(event.request).then(response => response || fetch(event.request))
	);
});