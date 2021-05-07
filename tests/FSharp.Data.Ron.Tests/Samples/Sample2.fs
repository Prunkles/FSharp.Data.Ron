module FSharp.Data.Ron.Tests.Samples.Sample2

open Expecto
open FSharp.Data.Ron

[<AutoOpen>]
module private Helpers =
    let rinteger = RonValue.Integer
    let rfloat = RonValue.Float
    let rstring = RonValue.String
    let rchar = RonValue.Char
    let rboolean = RonValue.Boolean
    let rmap items = RonValue.Map (Map.ofSeq items)
    let rlist = RonValue.List
    let runit = RonValue.AnyStruct AnyStruct.Unit
    let rtagged = AnyStruct.Tagged >> RonValue.AnyStruct
    let rnamed = AnyStruct.Named >> RonValue.AnyStruct
    let runnamed = AnyStruct.Unnamed >> RonValue.AnyStruct


let samples = [
    """
(
    boolean: true,
    float: 8.2,
    map: {
        1: '1',
        2: '4',
        3: '9',
        4: '1',
        5: '2',
        6: '3',
    },
    nested: Nested(
        a: "Decode me!",
        b: 'z',
    ),
    tuple: (3, 7),
    vec: [
        (a: "Nested 1", b: 'x'),
        (a: "Nested 2", b: 'y'),
        (a: "Nested 3", b: 'z'),
    ],
)
""",
    rnamed (None, [
        "boolean", rboolean true
        "float", rfloat 8.2
        "map", rmap [
            rinteger 1, rchar '1'
            rinteger 2, rchar '4'
            rinteger 3, rchar '9'
            rinteger 4, rchar '1'
            rinteger 5, rchar '2'
            rinteger 6, rchar '3'
        ]
        "nested", rnamed (Some "Nested", [
            "a", rstring "Decode me!"
            "b", rchar 'z'
        ])
        "tuple", runnamed (None, [ rinteger 3; rinteger 7 ])
        "vec", rlist [
            rnamed (None, [ "a", rstring "Nested 1"; "b", rchar 'x' ])
            rnamed (None, [ "a", rstring "Nested 2"; "b", rchar 'y' ])
            rnamed (None, [ "a", rstring "Nested 3"; "b", rchar 'z' ])
        ]
    ])

    """
Game(
    title: "Hello, RON!",
    level: Level( // We could just leave the `Level` out
        buildings: [
            (
                size: (10, 20),
                color: Yellow, // This as an enum variant
                owner: None,
            ),
            (
                size: (20, 25),
                color: Custom(0.1, 0.8, 1.0),
                owner: Some("guy"),
            ),
        ],
        characters: {
            "guy": (
                friendly: true,
            ),
        },
    ),
)
""",
    rnamed (Some "Game", [
        "title", rstring "Hello, RON!"
        "level", rnamed (Some "Level", [
            "buildings", rlist [
                rnamed (None, [
                    "size", runnamed (None, [ rinteger 10; rinteger 20 ])
                    "color", rtagged ("Yellow", false)
                    "owner", rtagged ("None", false)
                ])
                rnamed (None, [
                    "size", runnamed (None, [ rinteger 20; rinteger 25 ])
                    "color", runnamed (Some "Custom", [ rfloat 0.1; rfloat 0.8; rfloat 1.0 ])
                    "owner", runnamed (Some "Some", [ rstring "guy" ])
                ])
            ]
            "characters", rmap [
                rstring "guy", rnamed (None, [
                    "friendly", rboolean true
                ])
            ]
        ])
    ])
    
    """
GameConfig( // optional struct name
    window_size: (800, 600),
    window_title: "PAC-MAN",
    fullscreen: false,
    
    mouse_sensitivity: 1.4,
    key_bindings: {
        "up": Up,
        "down": Down,
        "left": Left,
        "right": Right,
        
        // Uncomment to enable WASD controls
        /*
        "W": Up,
        "A": Down,
        "S": Left,
        "D": Right,
        */
    },
    
    difficulty_options: (
        start_difficulty: Easy,
        adaptive: false,
    ),
)
""",
    rnamed (Some "GameConfig", [
        "window_size", runnamed (None, [ rinteger 800; rinteger 600 ])
        "window_title", rstring "PAC-MAN"
        "fullscreen", rboolean false
        "mouse_sensitivity", rfloat 1.4
        "key_bindings", rmap [
            rstring "up", rtagged ("Up", false)
            rstring "down", rtagged ("Down", false)
            rstring "left", rtagged ("Left", false)
            rstring "right", rtagged ("Right", false)
        ]
        "difficulty_options", rnamed (None, [
            "start_difficulty", rtagged ("Easy", false)
            "adaptive", rboolean false
        ])
    ])
    
    """
Scene( // class name is optional
    materials: { // this is a map
        "metal": (
            reflectivity: 1.0,
        ),
        "plastic": (
            reflectivity: 0.5,
        ),
    },
    entities: [ // this is an array
        (
            name: "hero",
            material: "metal",
        ),
        (
            name: "monster",
            material: "plastic",
        ),
    ],
)
""",
    rnamed (Some "Scene", [
        "materials", rmap [
            rstring "metal", rnamed (None, [
                "reflectivity", rfloat 1.0
            ])
            rstring "plastic", rnamed (None, [
                "reflectivity", rfloat 0.5
            ])
        ]
        "entities", rlist [
            rnamed (None, [
                "name", rstring "hero"
                "material", rstring "metal"
            ])
            rnamed (None, [
                "name", rstring "monster"
                "material", rstring "plastic"
            ])
        ]
    ])
]

[<Tests>]
let tests = test "Samples from docs" {
    for input, expected in samples do
        let ron = Parsing.parseFile input
        let actual = ron |> Result.map (fun r -> r.Value)
        Expect.equal actual (Ok expected) ""
}