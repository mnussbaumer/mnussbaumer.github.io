(function() {
    const themes = [["dark-theme", "dark", "light-theme", "light"], ["light-theme", "light", "dark-theme", "dark"]],
	  themes_values = themes.map(([theme,,,]) => theme),
	  default_theme = themes[0][0];    

    function current_theme() {
	return document.documentElement.getAttribute("theme");
    };

    function inverse_theme() {
	let current = current_theme();
	return themes.find(([theme,,,]) => theme != current);
    };

    function inverse_theme_label() {
	return inverse_theme()[1];
    };

    function setup_theme() {
	let saved_theme;
	
	try {
	    saved_theme = localStorage.getItem("mn-color-theme");
	    if (themes_values.includes(saved_theme)
		&& saved_theme != current_theme()) {
		
		set_theme(saved_theme);
		
	    } else if (window.matchMedia) {
		
		saved_theme = window.matchMedia("(prefers-color-scheme: dark)").matches ? "dark-theme" : "light-theme";
		
	    } else {
		saved_theme = current_theme() || default_theme;
		set_theme(saved_theme);
	    }
	    
	} catch (_) {
	    set_theme(default_theme);
	}
    }

    function set_theme(theme_string) {
	document.documentElement.setAttribute("theme", theme_string);
	
	let el = document.getElementById("theme-toggle");
	
	if (el) {
	    el.textContent = inverse_theme_label();
	}
	
	try {
	    return localStorage.setItem("mn-color-theme", theme_string);
	} catch (_) {}            
    }

    function toggle_theme() {
	let target_theme = inverse_theme();
	return set_theme(target_theme[0]);
    }

    document.addEventListener("DOMContentLoaded", () => {
	setup_theme();
	let btn_theme_toggle = document.getElementById("theme-toggle");
	
	if (btn_theme_toggle) {
	    btn_theme_toggle.onclick = () => toggle_theme();
	}
    });
})();

