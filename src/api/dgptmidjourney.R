# Load required packages
require(discordr)    # Package for interacting with Discord webhooks
require(chatgpt)     # Package for using ChatGPT API
require(glue)        # Package for working with strings

# Create a Discord connection object
conn_obj <-
  create_discord_connection(
    webhook = Sys.getenv("DISCORD_WEBHOOK"),    # Get Discord webhook URL from environment variable
    username = Sys.getenv("DISCORD_USER"),      # Get Discord username from environment variable
    set_default = TRUE                          # Set this connection as the default connection
  )
