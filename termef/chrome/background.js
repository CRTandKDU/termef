
let color = "<ul><li>TerMef est le gestionnaire des r\xE9f\xE9rentiels terminologiques des Minist\xE8res \xE9conomiques et financiers (Mef) au format de l'internet s\xE9mantique.</li></ul>";

chrome.runtime.onInstalled.addListener(() => {
    chrome.storage.sync.set({ color });
});

chrome.storage.onChanged.addListener(function (changes, namespace) {
    for (let [key, { oldValue, newValue }] of Object.entries(changes)) {
	console.log(
	    `Storage key "${key}" in namespace "${namespace}" changed.`,
	    `Old value was "${oldValue}", new value is "${newValue}".`
	);
    }
});
