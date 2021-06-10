---
layout: post
title: "ImageEngine with Next.js"
canonical_url: "https://dev.to/mnussbaumer/imageengine-with-nextjs-1dh7"
date: 2021-06-10 16:30:00 +0700
categories: react js imageengine nextjs
---

{: class="section-title" style="margin-top: 0;" }
#### Using ImageEngine in a React.js project


In this blog post we'll cover how to use ImageEngine in order to significantly improve your image assets, increasing your site responsiveness and improving your loading times - easy mode.

[You can check the final website we'll be building (requires JS, no cookies)](http://mnussbaumer-image-engine-sample.s3-website-us-west-2.amazonaws.com)

There's also a [repository](https://github.com/mnussbaumer/ie-nextjs-sample) containing all the code.

This post was originally published on [dev.to](https://dev.to/mnussbaumer/imageengine-with-nextjs-1dh7)

{: class="section-title" }
#### Why use ImageEngine (IE) to optimize your images?

While this post is particular to `Next.js` when exporting a static website - a case where `Next.js` inbuilt `Image Loader`, and its optimisations can't be used - `IE` only requires a query string on your image urls and can be used in any stack, even outside websites.

`Next.js` also has the capability of specifying custom image loaders for their inbuilt `<Image/>` component, and we can use those to reach an external CDN from which to load our images with our own logic - we'll go through that in another blog post.

If using `Next.js` with a `Nodejs` server and their inbuilt image optimisations, both the image optimisations and the serving of the assets will be done from the server side, while using `IE` none of it has to pass through your server with exception of the first serving - that coupled with the full range of customisable properties and highly optimised algorithms `IE` offers, makes it worth considering.

The only exception is in case you're deploying directly in `vercel.com`, their whole infra-structure supports `Next.js` inbuilt `Image Optimizations` (with automatic edge caching and good performance overall) which is good enough for most cases. Although even then, `IE` can provide significant bandwidth and improved response times savings over it, specially if you're serving a non trivial amount of images.

I'm going to walk you through implementing the website and deploying it on `S3`, using `IE` to optimise the delivery of its image content.

{: class="section-title" }
#### How can we use IE to improve our Nextjs app loading speed?


First let's go through the website so that you can make sense of what's happening and what we'll be building.

The website is designed to show the difference in file sizes, and consequently in loading times, between images distributed in their original versions and images distributed through `ImageEngine`'s smart `CDN`. Notice that the images that comprise the repository are sourced from the same files, but by using `IE` we can do some neat things with them.

![View of the final website](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/z2ejbeqyh5otbl9vmusa.jpg)

The thumbnails on the left are from images on the `IE CDN`, optimised through simple URL `directives`. This means that specifications such as `width`, `height`, `compression`, etc, are simply passed through query parameters on the URL the image tags use, without requiring any additional work. 

The thumbnails on the right side are from the original resources. 

If you click any thumbnail or file title from the sidebar, you'll see that for the `IE` side, the total size now increased to account for that, while the normal assets didn't - because they were already loaded. 

Of note is that many times, when loading a website, there's no actual need of loading full sized images, so optimising thumbnails is immediately an advantage in the majority of cases - because it allows a much faster initial load it impacts directly [Google’s Core Web Vitals](https://imageengine.io/blog/improve-core-web-vitals-with-imageengine) scoring.

In the same way the thumbnails were optimised through `directives`, we can also optimise those being displayed on the "preview" area. From the sidebar select, `Pick a type` -> `jpg`, `Pick a fit type` -> `cropbox` and enter `600` as the `width` and `400` as the `height directive`. There's others, like `compression`, `sharpness`, and so on that aren't on the sidebar, but available to use.

You should see some significant improvements on the size without any perceivable quality loss. At `600px X 400px`, it’s enough to cover retina when displaying images at `300px X 200px`, which is roughly the size of our previews.

Click around on the thumbnails to see the time images take to be displayed between changes. Even though the regular images are already downloaded and cached by the browser you can still see significant differences (`IE` images on first load, or when changing directives have to be fully processed and transferred, and usually still render faster than the unoptimized ones already downloaded and cached!). 

Let's start actually coding this.

{: class="section-title" }
#### Setting up the Next.js project

To follow through you'll need to have `nodejs` and `npm` installed. The versions used for this tutorial are `npm 6.14.4` and `node v10.19.0`.

On the folder where you'll be creating your project run:

`npx create-next-app`

Now let's install the image engine package:

`npm i @imageengine/react`

We should be able to start our app by doing

`npm run dev`

And visiting 

`http://localhost:3000`

should show us the default page. Let's remove the default templating `index.js` page has:


```js
import Head from "next/head";

export default function Home() {
  return (
      <div className="main-container">
        <Head>
          <title>ImageEngine Optimized Assets</title>
        </Head>
      </div>
  );
};
```

And change `/pages/_app.js` to the following

```js
import Head from "next/head";
import { ImageEngineProvider } from "@imageengine/react";
import "../styles/globals.css";

const delivery_address = process.env.DELIVERY_ADDRESS;

function LeanImages({ Component, pageProps }) {
    return (
	<ImageEngineProvider deliveryAddress={delivery_address}>
	  <Head>
	    <link rel="icon" type="image/png" href="/favicon.png"/>
	    <meta name="viewport" content="initial-scale=1.0, width=device-width" />
	    <meta name="description" content="Learn how to use ImageEngine in Nextjs to serve highly optimised image assets from your CDN to your users." />
	    <meta property="og:title" content="ImageEngine with NextJS" />
	    <meta property="og:description" content="Learn how to use ImageEngine in Nextjs to serve highly optimised image assets from your CDN to your users." />
	  </Head>
	  <Component {...pageProps} />
	</ImageEngineProvider>
    )
};

export default LeanImages;
```

If you have your dev server running you should see an error. Let's stop the server and create a `next.config.js` on the root of our project and inside it:

```js
module.exports = {
    env: {
	DELIVERY_ADDRESS: process.env.DELIVERY_ADDRESS || "http://localhost:3000",
	NON_OPTIMIZED_ADDRESS: process.env.NON_OPTIMIZED_ADDRESS || ""
    },
};
```

Let's remove the things we don't need, delete `pages/api` folder, `styles/Home.module.css` and `public/vercel.svg`::

`rm -rf pages/api`
`rm styles/Home.module.css`
`rm public/vercel.svg`

We should now have the following structure:

```
pages /
  _app.js
  index.js
styles /
  global.css
public /
  favicon.ico
```

We're ready to start. Our website will be a single page, divided into `2` sections, one left sidebar and a main area, the latter further divided into `3` parts - the title and image information, the preview area, and the thumbnails section. From the sidebar we'll be able to choose certain parameters related to the image display, and we'll have a small list with actionable items.

When clicking one of those items it will display them in the main area, in two versions, one from our image engine distribution, and the other from our original assets.

Locally it will source both from the same place in our assets, but once online correctly from their origins.

We will also display the total transferred file size for each image and a cumulative for all images loaded, between `optimized` and `non-optimized`. 

To share state between our components we’ll use `useReducer` so let's write its initial state, on our `pages/index.js`:

```js
const initial_settings_state = {
    base_path: "/images",
    file_types: ["jpg", "jp2", "webp"],
    file_type: null,
    fit_types: ["stretch", "box", "letterbox", "cropbox"],
    fit_type: null,
    width: null,
    height: null,
    images: [
    	"senja_norway.jpg",
    	"buddha_shakyamuni.jpg",
    	"vasnetsov_samolet.jpg",
    	"great_wave_off_kanagawa.jpg"
    ],
    copyright: {
    	"great_wave_off_kanagawa.jpg": ["Great Wave off Kanagawa", "By After Katsushika Hokusai - Restored version, Public Domain", ["https://commons.wikimedia.org/w/index.php?curid=5576388"]],
    	"buddha_shakyamuni.jpg": ["Buddha Shakyamuni", "By Unknown author - Public Domain", ["https://www.metmuseum.org/collection/the-collection-online/search/75274", "https://commons.wikimedia.org/w/index.php?curid=39112914"]],
    	"vasnetsov_samolet.jpg": ["The Flying Carpet", "By Viktor Mikhailovich Vasnetsov - belygorod.ru, Public Domain", ["https://commons.wikimedia.org/w/index.php?curid=1374733"]],
    	"senja_norway.jpg": ["Island of Senja, Troms, Norway (August, 2014)", "By Ximonic (Simo Räsänen) - Own work, CC BY-SA 4.0", ["https://commons.wikimedia.org/w/index.php?curid=34693021"]]
	
    },
    show_copyright: false,
    loaded_images_ie: {},
    loaded_images_regular: {},
    selected_image: null,
    selected_image_url: null,
    force_load: false
};
```

Our reducer function will be:

```js
function settings_reducer(state, action) {
    switch (action.type) {
    case "file_type":
    case "fit_type":
    case "width":
    case "height":
    case "force_load":
    case "show_copyright":
    	state[action.type] = action.value;
    	return {...state};
	
    case "selected_image":
    	state.selected_image = action.value;
    	state.selected_image_url = action.value ? `${state.base_path}/${action.value}` : null;
    	return {...state};

    case "add_image_size":
    	let state_key = build_type_key(action.image_type);
	
    	state[state_key][action.image] = action.size;
    	state[state_key] = {...state[state_key]};

    	return {...state};
    default:
        throw new Error();
  }
};
```

You might see an error because `build_type_key(action.image_type)` isn’t defined yet, we’ll add it in a while.

{: class="section-title" }
#### Sidebar component


Now our `sidebar` component. Create a folder `components` on the root folder and inside it a `sidebar.js` file:

```js
import React from "react";

export default function Sidebar({ state, dispatch }) {
    return (
	<div className="sidebar-wrapper">
	  <div className="sidebar">
	    <h2>Directives Optimizations</h2>
	    <select id="file_type" name="file_type" value={state.file_type || ""} onChange={(event) => dispatch({type: "file_type", value: event.target.value})}>
	      <option value="" >Pick a type</option>
	      {state.file_types.map((type, index) => {
                return  <option value={type} key={`file_type-${index}`}>{type}</option>
	      })}
	    </select>
	    <br/>
	    <br/>
	    <select id="fit_type" name="fit_type" value={state.fit_type || ""} onChange={(event) => dispatch({type: "fit_type", value: event.target.value})}>
	      <option value="">Pick a fit type</option>
	      {state.fit_types.map((type, index) => {
                return  <option value={type} key={`file_type-${index}`}>{type}</option>
	      })}
	    </select>
	    <br/>
	    <br/>
	    <input type="text" name="width" id="width" placeholder="Width directive in px, e.g: 25" value={state.width || ""} onChange={(event) => dispatch({type: "width", value: event.target.value})}/>
	    <br/>
	    <br/>
	    <input type="text" name="height" id="height" placeholder="Height directive in px, e.g: 50" value={state.height || ""} onChange={(event) => dispatch({type: "height", value: event.target.value})}/>
            <ul>
              {state.images.map((image, index) => {
                return <li key={`image-${index}`} className={`${image === state.selected_image ? "selected" : ""}`} onClick={() => dispatch({type: "selected_image", value: image})}>{image}</li>
               })}
            </ul>
	  </div>
	</div>
    );
};
```

  We have`select`s, `input`s and a list of elements. We set handlers to trigger on change for the inputs and dispatch the changes to our reducer, and click handlers on the list elements to dispatch the selection of an image.

  Regarding the styling, to save some space I’ll link directly to the [final css file](https://github.com/mnussbaumer/ie-nextjs-sample/blob/main/styles/globals.css). Either copy the whole file or otherwise add it piece by piece as I’ll mention the initial line (`IL`) and last line (`LL`) for each component. In this case `IL0 - LL128`.

  The central part will be made of two sections, the top part - info and images previews - and the bottom thumbnail section.
Create three files on the `components` folder, `copyright.js`, `thumbnails.js` and `image_preview.js`.

{: class="section-title" }
#### Copyright component
On `copyright.js`:


```js
export default function Copyright({ state, dispatch }) {
    if (!state.selected_image) {
	return <div className="main-area-image-info"></div>;
    } else {
    	let copyright = state.copyright[state.selected_image],
    	    title = copyright[0],
    	    description = copyright[1],
    	    links = copyright[2];

    	return (
    	    <div className="main-area-image-info">
    	      <h4>{title}<button className="collapse-btn" type="button" onClick={() => dispatch({type: "show_copyright", value: !state.show_copyright})}>{state.show_copyright ? "-" : "+"}</button></h4>
    	      {state.show_copyright ? <div>
	             	    <p>{description}</p>
     			{links.map((link, index) => <a href={link} target="_blank" key={`link-${state.selected_image}-${index}`}>{link}<br/></a>)}
    	       </div> :
    	       null}
    	    </div>
    	)
    }
};
```

`styles/globals.css` add from `IL131 - LL166`.

{: class="section-title" }
#### Thumbnails component
`thumbnails.js`

```js
import { Image } from "@imageengine/react";
import { get_size, handle_image, NON_OPTIMIZED_ADDRESS } from "../js/utilities";

export default function Thumbnails({ state, dispatch }) {
    
    return (
	<div className="main-area-thumbnails">
	  <div className="thumbnails-container optimized">
	    {state.force_load && state.images.map((image, index) => {
                 let image_path = `${state.base_path}/${image}`,
                     thumb_id = `thumb_${image_path}`;
                 return (
                    <div className="thumbnail-image" key={`thumbnail-optimized-${index}`} onClick={() => dispatch({type: "selected_image", value: image})}>
                      <div className="thumbnail-size">
                        {get_size(state, "ie", thumb_id)}
                      </div>
                      <Image
                        src={image_path}
                        directives={{
                          outputFormat: "jpg",
                          height: 90,
                          width: 90,
                          fitMethod: "cropbox",
                          compression: 80
                        }}
                        onLoad={(event) => handle_image("ie", event.target, thumb_id, dispatch)}
                      />
		    </div>
		);
	    })}
	  </div>
	  <div className="thumbnails-container regular">
            {state.force_load && state.images.map((image, index) => {
              let image_path = `${NON_OPTIMIZED_ADDRESS}${state.base_path}/${image}`;
              return (
                  <div className="thumbnail-image" key={`thumbnail-regular-${index}`} onClick={() => dispatch({type: "selected_image", value: image})}>
                     <div className="thumbnail-size">
                        {get_size(state, "regular", image_path)}
                     </div>
                     <img src={image_path} onLoad={(event) => handle_image("regular", event.target, image_path, dispatch)} />
                  </div>
                );
	    })}
	  </div>
	</div>
    )
};
```

  Here we'll use the `Image` component provided by the `@imageengine/react` package, and import a bunch of helper functions not yet defined. 
  We have two containers, on each of these containers we map the `images` we have to image elements - on the `optimized` one we build the full path, and create a unique id for this thumbnail. We display the size of that image with `get_size` and use IE’s `Image` component to build a custom image source.

  On the `regular` one we do the same mapping but use a bare `<img>` tag. In these we don't create a `thumb_id` because the images will be these, but  on the `optimized` ones in order to account correctly for the total size of files being loaded we need to differentiate between the `thumbnails` retrieved from `IE` and the previews.
  We’re using `state.force_load` to control the rendering, this is a walk-around for a glitch that happens on first load where sometimes the thumbnails won’t trigger the loaded handler.


{: class="section-title" }
#### Helper functions


For the helper functions, create a directory named `js`, and inside it a file named `utilities.js`:

```js
export const NON_OPTIMIZED_ADDRESS = process.env.NON_OPTIMIZED_ADDRESS;

export async function handle_image(image_type, img, selected_image_url, dispatch) {

    let head = await fetch(img.src, {method: "head"}),
        size = head.headers.get("content-length");

    if (size) {
      dispatch({type: "add_image_size", image_type: image_type, image: selected_image_url, size: parseInt(size)});
    }
}

export function get_size(state, type, url) {
    let size = state[build_type_key(type)][url];
    return  size || size === 0 ? display_size(size) : "No info";
};

export function calc_total_sizes(state, type) {
    let total_size = Object.values(state[build_type_key(type)]).reduce((acc, size) => acc + size, 0);
    return display_size(total_size);
};

export function build_type_key(type) {
    return `loaded_images_${type}`;
};

function display_size(size) {
    if (size > 1048576) {
      return (Math.round((size / 1048576 + Number.EPSILON) * 100) / 100) + "MB";
    } else if (size > 1024) {
      return (Math.round((size / 1024 + Number.EPSILON) * 100) / 100) + "Kb";
    } else {
      return size + "b";
    }
};
```

Let's go through each element here:
* `NON_OPTIMIZED_ADDRESS` just proxies the value we will set as an `ENV` variable, to be picked from our `next.config.js` we created earlier.
* `handle_image` is the function we're using when an image finishes loading. It takes 4 arguments:
   * `image_type` - if it's `ie` or `regular` so we can distinguish between which kind of image was loaded 
   * `img` - which is the `html img` element itself
   * `selected_image_url` - which is the `key` under which we to correlate with this image
   * `dispatch` - which is our reducer's dispatch function.

Now, there isn't a straightforward way in JS to get an image size that works every time for local, cross-origin, and cached files, so our solution is to issue an `head` request for the same resource. We use the actual `img` element to retrieve the exact `src` attribute. The response to this request includes the size of the resource, under the `content-length` header, but without actually transferring its content.  

* `get_size` is an helper that retrieves the correct size, for a particular url of a given type from our `state`.
* `calc_total_sizes` sums up the sizes of all resources we have retrieved for a given type.
* `build_type_key` is a simple helper to return the correct state key for a given type.
* `display_size` is a helper that formats an integer value representing bytes into either `mb`, `kb` or `b` 

With this in place, let's add the css for our thumbnails section, `IL169 - LL236`

{: class="section-title" }
#### Image Preview component

Finally, on `image_preview.js`:

```js
import { get_size, calc_total_sizes } from "../js/utilities.js";

export default function ImagePreview({ state, type, prefix,  children }) {
    let prefixed = `${prefix}${state.selected_image_url}`;
    
    return (
	<div className="image-container-wrapper">
	  <div className="image-info-container">
	    <div className="image-info-size">{state.selected_image_url ? get_size(state, type, prefixed) : "No info"}</div>
	    <div className="image-info-size-total">Total: {calc_total_sizes(state, type)}</div>
	  </div>
	  <div className="image-container">
            {state.selected_image ?
              children :
              <p>Select an Image to preview</p>
	    }
	  </div>
	</div> 	    
    );
};
```

This one is a simple wrapper. If there's no selected image it shows `Select an image to preview`, and in case the size of the file hasn't been retrieved yet (it's an async function) it shows `No info`, if there's a selected image we use the children prop to display it. 


Let's add the CSS styling for this part `IL239 - LL336`

{: class="section-title" }
#### Final index.js

Lastly, we're ready to finish our `index.js` file (omitting the reducer and initial state) :

```js
import Head from "next/head";
import Sidebar from "../components/sidebar.js";
import ImagePreview from "../components/image_preview.js";
import Thumbnails from "../components/thumbnails.js";
import Copyright from "../components/copyright.js";

import { Image } from "@imageengine/react";

import { useReducer, useEffect } from "react";

import {
    build_type_key,
    handle_image,
    NON_OPTIMIZED_ADDRESS
} from "../js/utilities.js";


export default function Home() {
    const [state, dispatch] = useReducer(settings_reducer, initial_settings_state);

    useEffect(() => {
	setTimeout(() => dispatch({type: "force_load", value: true}), 25)
    }, []);
    
    return (
	<div className="main-container">
	  <Head>
	    <title>ImageEngine Optimized Assets</title>
	  </Head>
	  <Sidebar state={state} dispatch={dispatch} />
	  <div className="main-area">
	    <h2 className="page-title">ImageEngine (left) vs Unoptimized (right)</h2>
	    <Copyright state={state} dispatch={dispatch} />
	    <div className="main-area-images">
              <ImagePreview state={state} type="ie" prefix={""}>
                <Image
                  src={state.selected_image_url}
                  directives={{
                    outputFormat: state.file_type,
                    height: state.height,
                    width: state.width,
                    fitMethod: state.fit_type
                  }}
                  onLoad={(event) => handle_image("ie", event.target, state.selected_image_url, dispatch)}
                />
              </ImagePreview>
              <ImagePreview state={state} type="regular" prefix={NON_OPTIMIZED_ADDRESS}>
                <img src={NON_OPTIMIZED_ADDRESS + state.selected_image_url} onLoad={(event) => handle_image("regular", event.target, (NON_OPTIMIZED_ADDRESS + state.selected_image_url), dispatch)}/>
              </ImagePreview>
	    </div>
	    <Thumbnails state={state} dispatch={dispatch} />
	  </div>
	</div>
    );
};
```

Besides the `useReducer` we also imported `useState` and used it here:
```
  useEffect(() => {
    	setTimeout(() => dispatch({type: "force_load", value: true}), 25)
    }, []);
```
To show the thumbnails so their `onload` handle triggers properly.

On the first `<ImagePreview>` component we pass as a child an ImageEngine `Image` component, and we set both the `src` prop to our `selected_image_url` and the `directives` prop to an object containing the customisable properties we have in our sidebar. We could add further directives here. Because we wrapped our `_app.js` content with an `<ImageEngineProvider>` it will automatically be used by the `<Image>` component. In a normal situation we might want to debounce the changes to our reducer’s state so it doesn’t immediately request a new image.

In the second `<ImagePreview>` instead we pass a simple html `<img>` tag.

If you try to see the website it should be working, but the images won't be loading as we haven't them yet - you can download them from [images](https://github.com/mnussbaumer/ie-nextjs-sample/tree/main/public/images) and place them in `public/images`.
We aren't using yet an actual `IE` distribution, so we won't have any differences in sizes, except that the IE side will count each image twice as you pick them, once for the thumbnails, another for the previews - as we discussed earlier.

If you want to add responsiveness to the layout, there’s a few more lines of css to add, `IL338 - LL372`.

{: class="section-title" }
#### Bucket, IE distribution, static build and deploying

Now that we have our app ready, let's set up an AWS S3 bucket to host it. In this case we will host it bare on S3, as a static website, making the whole bucket public, but outside of a tutorial, this bucket shouldn't be public at all and instead use cloudfront in front (get it?). You can do it from the aws console interface, or using the aws cli. With the cli, change the `bucket-name` and ` use-west-2` region to something you want on the following commands:

`aws s3 mb s3://bucket-name --region us-west-2`
`aws s3 website s3://bucket-name/ --index-document index.html --error-document 404.html`

Because we created the bucket in `us-west-2` region and set it as a static website, it will be hosted on the following url `http://bucket-name.s3-website-us-west-2.amazonaws.com`

If using AWS Console just create a new bucket and set it as a static website after it's created (from the `Properties` tab),

Now that we have an host we can refer to, let's create an ImageEngine distribution for it:

<video width="100%" controls ><source src="https://cdn.imageengine.io/images/docs/IE-signup.mp4" type="video/mp4">Your browser does not support the video tag.</video>

[imageengine.io](https://imageengine.io)

changing the url address used in the video to that of your bucket. Then from your account panel, select `Edit Engine` and on the `CORS Support` section change the `HTTP methods allowed` value from `GET`, to `GET,HEAD`.

![setting cors in IE](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/32xo66dxc7hu2kmor8nf.jpg)

And that’s all for `IE`. Right now we don't have any files on our bucket so we don't have a website yet. `Next.js` allows you to build a static website that we can use in `S3`. Since we have the url for our bucket and for our IE distribution we can build our website using those values.

Let's add a specific command to our `package.json` that does it for us, under the `scripts` key, add `"static": "next build && next export"`. Save the file.
 From the command line we can now run (substituting the values for the ones you have):

`DELIVERY_ADDRESS="https://your-image-engine.cdn.imgeng.in" NON_OPTIMIZED_ADDRESS="http://bucket-name.s3-website-us-west-2.amazonaws.com" npm run static`

This will create all necessary files for a static website in the `out` directory of your app dir. The last step is to upload those files into our bucket. We can do that manually through the console interface, or using the cli:

`aws s3 cp out s3://bucket-name/ --recursive --acl public-read`

Notice we need to set the Access Control List to `public-read`. If doing the upload through the interface we would need to set it on the upload panel, before uploading them.

And that's it, maybe give it a minute and you should be able to visit `http://bucket-name.s3-website-us-west-2.amazonaws.com` and see it working!

Now that you have the ImageEngine distribution available you can also run it locally with `DELIVERY_ADDRESS="https://your-image-engine.cdn.imgeng.in" npm run dev`.

If you omit the `DELIVERY_ADDRESS` then it will serve the local assets for both parts as it did initially. This is the same if you're integrating ImageEngine into an existing website, you can do all the code changes to use the `Image` component, while still relying on the normal image urls, and once ready, build a version with the correct `DELIVERY_ADDRESS` (or if running on a nodejs server restart it with the proper env set). If you’re not using Nextjs, or need some more control on your image components, you can also build the query string for the directives and use it directly in any `img` element’s `src`, or in http requests.

{: class="section-title" }
#### Conclusion

It's easy to add **IE** to a website or when starting a new project, as it involves just attaching a query string parameter to your image `src`'s. In the same way it's also easy to leverage new HTML image attributes, such as `srcset/sizes` to define particular viewport dimensions with specific optimisations, as again, it just requires a query string.

If you're serving a significant amount of images and you don't have an image optimisation pipeline in place, need finer grain control than things like `Next.js` image optimisations, aren't running a `Nodejs` web server, or don't want to worry about that part of your infrastructure, then `IE` makes a lot of sense - its `CDN` is **really fast** as is their on-the-fly transformation engine - on top of that given their work on adjacent areas - *device display identification* - any improvements to their algorithms are automatically applied to your existing sources, and new options when available easily integrated.
