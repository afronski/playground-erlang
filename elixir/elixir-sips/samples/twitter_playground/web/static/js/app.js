import {Socket} from "phoenix";

class App {
    static init() {
        let socket = new Socket("/ws");

        socket.connect();
        socket.join("tweets", { query: "apple" }).receive("ok", channel => {
            console.log("Channel:", channel);

            channel.on("new_tweet", message => {
                console.info("Message:", message);

                let li = document.createElement("li");

                let text = document.createTextNode(message.text);
                li.appendChild(text);

                document.querySelector("#tweets").appendChild(li);
            });
        });
    }
}

document.addEventListener("DOMContentLoaded", () => App.init());
