import std.experimental.logger;

import gfm.opengl,
       gfm.sdl2,
       gfm.core;

import std.typecons,
       std.string;

import vibe.d;

import fs;

import std.stdio;
import std.concurrency: spawn;

/* import backtrace.backtrace; */

shared static this()
{
    auto settings = new HTTPServerSettings;
    settings.port = 8080;
    settings.bindAddresses = ["::1", "127.0.0.1"];
    listenHTTP(settings, &hello);
    logInfo("Please open http://127.0.0.1:8080/ in your browser.");
}

void hello(HTTPServerRequest req, HTTPServerResponse res)
{
    res.writeBody("Hello, World!");
}

void req()
{
    requestHTTP("http://www.google.com/",
                (scope req) {},
                (scope res) {
                    logInfo("Response: %d", res.statusCode);
                    foreach (k, v; res.headers)
                        logInfo("Header: %s: %s", k, v);
                });
}

void gfmMain(immutable string[] args)
{
    /* import std.stdio: stderr; */
    /* backtrace.backtrace.install(stderr); */

    if (false) {
        scanner(args.dup);
    }

    /* req(); */

    int width = 800;
    int height = 600;

    // create a logger
    auto log = new ConsoleLogger();

    auto sdl = scoped!SDL2(log);
    auto gl  = scoped!OpenGL(log);

    /*
      Flags:
      SDL_WINDOW_FULLSCREEN, SDL_WINDOW_OPENGL,
      SDL_WINDOW_HIDDEN,        SDL_WINDOW_BORDERLESS,
      SDL_WINDOW_RESIZABLE,     SDL_WINDOW_MAXIMIZED,
      SDL_WINDOW_MINIMIZED,     SDL_WINDOW_INPUT_GRABBED,
      SDL_WINDOW_ALLOW_HIGHDPI.
    */
    int flags = SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE | SDL_WINDOW_MAXIMIZED;
    auto window = scoped!SDL2Window(sdl,
                                    SDL_WINDOWPOS_UNDEFINED,
                                    SDL_WINDOWPOS_UNDEFINED,
                                    width,
                                    height,
                                    flags);
    gl.reload();

    //standard OpenGL calls
    glViewport(0, 0, width, height);
    glClearColor(0, 0, 0, 1);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    alias Position3 = GLfloat[3];
    alias Color3 = GLfloat[3];

    struct Vertex3
    {
        Position3 position;
        Color3 color;
    }

    Vertex3[8] hexFanVertices = [ { [   0,   0, 0], [1, 1, 1] },
                                  { [   0,   1, 0], [0, 1, 0] },
                                  { [ 0.5, 0.5, 0], [0, 1, 0] },
                                  { [ 0.5,-0.5, 0], [0, 1, 0] },
                                  { [   0,  -1, 0], [0, 1, 0] },
                                  { [-0.5,-0.5, 0], [0, 1, 0] },
                                  { [-0.5, 0.5, 0], [0, 1, 0] },
                                  { [   0,   1, 0], [0, 1, 0] } ];

    // SQUARE

    Vertex3[4] squareVertices = [ { [0, 0, 0], [1, 1, 1] },
                                  { [1, 0, 0], [0, 1, 1] },
                                  { [1, 1, 0], [0, 0, 1] },
                                  { [0, 1, 0], [.5, .5, .5] } ];

    GLuint[6] squareIndices = [0, 1, 2, 0, 2, 3];

    auto squareVS = scoped!VertexSpecification(gl);

    // create and bind the buffer used by the square vertices.
    squareVS.VBO = new GLBuffer(gl, GL_ARRAY_BUFFER, GL_STATIC_DRAW, squareVertices[]);
    scope(exit) squareVS.VBO.close();

    // create and bind the buffer used by the square indices.
    squareVS.IBO = new GLBuffer(gl, GL_ELEMENT_ARRAY_BUFFER, GL_STATIC_DRAW, squareIndices[]);
    scope(exit) squareVS.IBO.close();

    // Compiles the shaders for the square.
    auto squareProgram = scoped!GLProgram(gl,
        r"#version 110
        #if VERTEX_SHADER

        void main()
        {
            gl_FrontColor = gl_Color;
            gl_Position = vec4(0.5, 0.5, 0.5, 1) * gl_Vertex;
        }

        #elif FRAGMENT_SHADER

        void main()
        {
            gl_FragColor = gl_Color;
        }

        #endif
        ");

    // Add attributes for the square: position and color with "legacy" code (OpenGL 2.0 style), 3 floats each.
    // Variables will be accessible in the shader by 'gl_Vertex' and 'gl_Color' variables
    squareVS.addLegacy(VertexAttribute.Role.POSITION, GL_FLOAT, 3);
    squareVS.addLegacy(VertexAttribute.Role.COLOR, GL_FLOAT, 3);

    // TRIANGLE

    Vertex3[3] triangleVertices = [ { [-0.5, -0.5, 0], [1, 0.5, 0] },
                                   { [ 0.5, -0.5, 0], [0.5, 1, 0] },
                                   { [   0,  0.5, 0], [1, 1, 0] } ];

    // Creates and binds the buffer used by the triangle vertices.
    // Please note: we will NOT use indices for the triangle
    auto triangleVBO = scoped!GLBuffer(gl, GL_ARRAY_BUFFER, GL_STATIC_DRAW, triangleVertices[]); // this buffer will hold the vertex data

    // Compiles the shaders for the triangle.
    auto triangleProgram = scoped!GLProgram(gl,
        r"#version 110

        varying vec4 color;

        #if VERTEX_SHADER

        attribute vec4 color_attribute;
        uniform float angle;

        void main()
        {
            color = color_attribute;
            mat4 rot = mat4( cos(angle), -sin(angle), 0.0, 0.0,
                             sin(angle), cos(angle),  0.0, 0.0,
                             0.0       , 0.0,         1.0, 0.0,
                             0.0       , 0.0,         0.0, 1.0 );

            gl_Position = rot * ( vec4(0.5, 0.5, 0.5, 1.0) * gl_Vertex );
        }

        #elif FRAGMENT_SHADER

        void main()
        {
            gl_FragColor = vec4(color.xyz, 0.7);
        }

        #endif
        ");

    // add one attribute to the triangle: position, as "legacy" Role.POSITION (OpenGL 2.0 style);
    // add another attribute: color, as GENERIC attribute (OpenGL 3.0+ style); the color is added by attribute name
    // Variables will be accessible in the shader by 'gl_Vertex' and 'color_attribute' respectively
    auto triangleVS = scoped!VertexSpecification(gl);
    triangleVS.addLegacy(VertexAttribute.Role.POSITION, GL_FLOAT, 3);
    triangleVS.addGeneric(GL_FLOAT, 3, "color_attribute");

    // HEXAGON

    auto hexVS = scoped!VertexSpecification(gl);
    // create and bind the buffer used by the hexagon vertices.
    hexVS.VBO = new GLBuffer(gl, GL_ARRAY_BUFFER, GL_STATIC_DRAW, hexFanVertices[]);
    scope(exit) hexVS.VBO.close();

    // Compiles the shaders for the hexagon.
    // Note: OpenGL 3 + extensions / OpenGL3.3 required for this shader.
    // Pass the color to the post-vertex and to the fragment shader.
    auto hexProgram = scoped!GLProgram(gl,
        format(
        r"#version %s

        #if VERTEX_SHADER

        #extension GL_ARB_explicit_attrib_location : enable

        layout(location = 0) in vec4 position_attribute;
        layout(location = 1) in vec4 color_attribute;
        out vec4 out_color;
        void main()
        {
          out_color = color_attribute; //pass the color to the post-vertex and to the fragment shader
          gl_Position = vec4(0.5, 0.4, 1, 1) * position_attribute + vec4(-0.4, 0.4, 0, 0);
        }

        #elif FRAGMENT_SHADER

        in vec4 out_color;
        out vec4 final_color;
        void main()
        {
            final_color = out_color;
        }

        #endif
        ", gl.getVendor() == OpenGL.Vendor.NVIDIA ? "330" : "130" ));

    // Add attributes for the hexagon: position and color as GENERIC attributes (OpenGL 3.0+ style), 3 floats each
    // both are added by attribute location (the location is fixed in the shader via "layout(location = N) in ...")
    hexVS.addGeneric(GL_FLOAT, 3, 0);
    hexVS.addGeneric(GL_FLOAT, 3, 1);

    double time = 0;

    // The FrameCounter object gives the clock and maintain statistics about framerate.
    auto frameCounter = scoped!FrameCounter(sdl);

    /* While the program is running */
    while(!sdl.keyboard().isPressed(SDLK_ESCAPE))
    {
        sdl.processEvents();

        time += frameCounter.tickSecs();

        // clear the whole window
        glClear(GL_COLOR_BUFFER_BIT);

        // draw the square
        squareVS.use();         // use this VertexSpecification
        squareProgram.use();    // use the square shader program
        glDrawElements(GL_TRIANGLES,
                       cast(int)(squareVS.IBO.size() / uint.sizeof),
                       GL_UNSIGNED_INT,
                       cast(void*)0);
        squareProgram.unuse();  // unuse this VertexSpecification
        squareVS.unuse();       // unuse the square shader program

        // draw the hexagon
        hexProgram.use();       // use the hexagon shader program
        hexVS.use();            // use this VertexSpecification
        glDrawArrays(GL_TRIANGLE_FAN,
                     0,
                     cast(int)(hexVS.VBO.size() / hexVS.vertexSize()));
        hexProgram.unuse();     // unuse th VertexSpecification
        hexVS.unuse();          // unuse the shader program

        // draw the triangle
        triangleVBO.bind();     // manually bind the VBO
        triangleProgram.uniform("angle").set!float(time * 0.4);
        triangleProgram.use();
        triangleVS.use(triangleProgram);
        glDrawArrays(GL_TRIANGLES,
                     0,
                     cast(int)(triangleVBO.size() / triangleVS.vertexSize()));
        triangleProgram.unuse();
        triangleVS.unuse();

        window.setTitle("Test: a green hexagon, a blue rectangle, a yellow transparent triangle");
        window.swapBuffers();
    }
}

void smallMain()
{
    writeln("Entered ", __FUNCTION__);
    // assert(false);
    writeln("Exiting ", __FUNCTION__);
}

void main(string[] args)
{
    writeln("Entered ", __FUNCTION__);
    auto smallMainTid = spawn(&smallMain);
    auto otherMainTid = spawn(&gfmMain, args.idup);
    runEventLoop();
    writeln("Exiting ", __FUNCTION__);
}
