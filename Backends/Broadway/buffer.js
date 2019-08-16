const BLOCK_SIZE        = 32;

const TRANSPARENT_PIXEL = 0x00;
const DELTA_0_RUN       = 0x10;
const BLOCK_REFERENCE   = 0x20;
const COLOR_RUN         = 0x30;
const DELTA_RUN         = 0x40;

function copyRect(src, srcX, srcY, dest, destX, destY, width, height)
{
    // Clip to src
    if (srcX + width > src.width)
        width = src.width - srcX;
    if (srcY + height > src.height)
        height = src.height - srcY;

    // Clip to dest
    if (destX + width > dest.width)
        width = dest.width - destX;
    if (destY + height > dest.height)
        height = dest.height - destY;

    var srcRect = src.width * 4 * srcY + srcX * 4;
    var destRect = dest.width * 4 * destY + destX * 4;

    for (var i = 0; i < height; i++) {
        var line = src.data.subarray(srcRect, srcRect + width *4);
        dest.data.set(line, destRect);
        srcRect += src.width * 4;
        destRect += dest.width * 4;
    }
}


function markRun(dest, start, length, r, g, b)
{
    for (var i = start; i < start + length * 4; i += 4) {
        dest[i+0] = dest[i+0] / 2 | 0 + r;
        dest[i+1] = dest[i+1] / 2 | 0 + g;
        dest[i+2] = dest[i+2] / 2 | 0 + b;
    }
}

function markRect(src, srcX, srcY, dest, destX, destY, width, height, r, g, b)
{
    // Clip to src
    if (srcX + width > src.width)
        width = src.width - srcX;
    if (srcY + height > src.height)
        height = src.height - srcY;

    // Clip to dest
    if (destX + width > dest.width)
        width = dest.width - destX;
    if (destY + height > dest.height)
        height = dest.height - destY;

    var destRect = dest.width * 4 * destY + destX * 4;

    for (var i = 0; i < height; i++) {
        if (i == 0 || i == height-1)
            markRun(dest.data, destRect, width, 0, 0, 0);
        else {
            markRun(dest.data, destRect, 1, 0 ,0, 0);
            markRun(dest.data, destRect+4, width-2, r, g, b);
            markRun(dest.data, destRect+4*width-4, 1, 0, 0, 0);
        }
        destRect += dest.width * 4;
    }
}

function decodeBuffer(context, oldData, w, h, data, debug)
{
    var i, j;
    var imageData = context.createImageData(w, h);

    if (oldData != null) {
        // Copy old frame into new buffer
        copyRect(oldData, 0, 0, imageData, 0, 0, oldData.width, oldData.height);
    }

    var src = 0;
    var dest = 0;

    while (data.pos < (data.length - 20))  {
        var b = data.get_uint8();
        var g = data.get_uint8();
        var r = data.get_uint8();
        var alpha = data.get_uint8();
        var len, start;

        if (alpha != 0) {
            // Regular data is red
            if (debug) {
                r = r / 2 | 0 + 128;
                g = g / 2 | 0;
                b = r / 2 | 0;
            }

            imageData.data[dest++] = r;
            imageData.data[dest++] = g;
            imageData.data[dest++] = b;
            imageData.data[dest++] = alpha;
        } else {
            var cmd = r & 0xf0;
            switch (cmd) {
            case TRANSPARENT_PIXEL:
                //log("Got transparent");
                imageData.data[dest++] = 0;
                imageData.data[dest++] = 0;
                imageData.data[dest++] = 0;
                imageData.data[dest++] = 0;
                break;

            case DELTA_0_RUN:
                len = (r & 0xf) << 16 | g << 8 | b;
                //log("Got delta0, len: " + len);
                dest += len * 4;
                break;

            case BLOCK_REFERENCE:
                var blockid = (r & 0xf) << 16 | g << 8 | b;

                var block_stride = (oldData.width + BLOCK_SIZE - 1) / BLOCK_SIZE | 0;
                var srcY = (blockid / block_stride | 0) * BLOCK_SIZE;
                var srcX = (blockid % block_stride | 0) * BLOCK_SIZE;

                var destX = data.get_16();
                var destY = data.get_16();

                copyRect(oldData, srcX, srcY, imageData, destX, destY, BLOCK_SIZE, BLOCK_SIZE);
                if (debug) // blocks are green
                    markRect(oldData, srcX, srcY, imageData, destX, destY, 32, 32, 0x00, 128, 0x00);

                //log("Got block, id: " + blockid +  "(" + srcX +"," + srcY + ") at " + destX + "," + destY);

                break;

            case COLOR_RUN:
                len = (r & 0xf) << 16 | g << 8 | b;
                // log("Got color run, len: " + len);

                b = data.get_uint8();
                g = data.get_uint8();
                r = data.get_uint8();
                alpha = data.get_uint8();

                start = dest;

                for (i = 0; i < len; i++) {
                    imageData.data[dest++] = r;
                    imageData.data[dest++] = g;
                    imageData.data[dest++] = b;
                    imageData.data[dest++] = alpha;
                }

                if (debug) // Color runs are blue
                    markRun(imageData.data, start, len, 0x00, 0x00, 128);

                break;

            case DELTA_RUN:
                len = (r & 0xf) << 16 | g << 8 | b;
                //log("Got delta run, len: " + len);

                b = data.get_uint8();
                g = data.get_uint8();
                r = data.get_uint8();
                alpha = data.get_uint8();

                start = dest;

                for (i = 0; i < len; i++) {
                    imageData.data[dest] = (imageData.data[dest] + r) & 0xff;
                    dest++;
                    imageData.data[dest] = (imageData.data[dest] + g) & 0xff;
                    dest++;
                    imageData.data[dest] = (imageData.data[dest] + b) & 0xff;
                    dest++;
                    imageData.data[dest] = (imageData.data[dest] + alpha) & 0xff;
                    dest++;
                }
                if (debug) // Delta runs are violet
                    markRun(imageData.data, start, len, 0xff, 0x00, 0xff);
                break;

            default:
                alert("Unknown buffer commend " + cmd);
            }
        }
    }
    data.pos = data.length; // TODO temp

    return imageData;
}

function cmdPutBuffer(id, w, h, compressed)
{
    var surface = surfaces[id];
    var context = surface.canvas.getContext("2d");

    var inflate = new Zlib.RawInflate(compressed);
    var data = inflate.decompress();

    var imageData = decodeBuffer (context, surface.imageData, w, h, data, debugDecoding);
    context.putImageData(imageData, 0, 0);

    if (debugDecoding)
        imageData = decodeBuffer (context, surface.imageData, w, h, data, false);

    surface.imageData = imageData;
}
