import Config

config :tableau, :config, url: "https://micaelnussbaumer.com"
config :tableau, Tableau.PostExtension, future: false, dir: ["_posts"]
config :tableau, Tableau.PageExtension, dir: ["_pages"]
