'=========================================
' Teenage Driving Simulator (QB64)
' MandO presents..
'
' Controls:
'   W or SPACE = accelerate
'   S or X     = brake
'   A          = steer left
'   D          = steer right
'   ESC        = quit
'
' Goal:
'   Keep COLLISIONS low.
'   (Bonus: near-miss "STYLE" points)
'=========================================

Option _Explicit

Const SW = 800
Const SH = 600

Const MAXP = 5
Const MAXT = 22

Const STARCOUNT = 160
Const BOOTLINES = 7

Dim Shared fontLarge&, fontSmall&

' Title screen shared stuff (prevents duplicate-definition popups)
Dim Shared titleInit% ' one-time init
Dim Shared titleStart! ' resets on enter title
Dim Shared starX(1 To STARCOUNT) As Single
Dim Shared starY(1 To STARCOUNT) As Single
Dim Shared starV(1 To STARCOUNT) As Single
Dim Shared starA(1 To STARCOUNT) As Integer
Dim Shared bootLine(1 To BOOTLINES) As String

' Obstacles (pedestrians)
Dim Shared pedActive(1 To MAXP) As Integer
Dim Shared pedX(1 To MAXP) As Single
Dim Shared pedZ(1 To MAXP) As Single
Dim Shared pedVX(1 To MAXP) As Single
Dim Shared pedType(1 To MAXP) As Integer
Dim Shared pedPhase(1 To MAXP) As Single
Dim Shared pedScored(1 To MAXP) As Integer

' Trees
Dim Shared treeZ(1 To MAXT) As Single
Dim Shared treeSide(1 To MAXT) As Integer
Dim Shared treeVar(1 To MAXT) As Single

DECLARE SUB InitFonts ()
DECLARE SUB TitleScreen ()
DECLARE SUB GameLoop ()

DECLARE SUB DrawScanlines ()
DECLARE SUB DrawSteeringWheel (cx!, cy!, r!, angDeg!, col&)

DECLARE SUB DrawHUD (dashTop%, speed!, rpm!, fuel!, temp!, gear$, collisions&, odom!, steerAng!, laneX!, yawRate!, style&, sigL%, sigR%, warnLowFuel%, warnHot%, warnOver%, skidWarn%, colBright&, colDim&, colGlass&)
DECLARE SUB DrawBarH (x%, y%, w%, h%, t!, colBright&, colDim&, colGlass&)
DECLARE SUB DrawBarV (x%, y%, w%, h%, t!, colBright&, colDim&, colGlass&)
DECLARE SUB DrawPed (px!, py!, f!, pType%, phase!, colBright&, colDim&)

DECLARE SUB SpawnPed (i%, startZ!)
DECLARE SUB SpawnTree (i%, startZ!)
DECLARE FUNCTION Clamp! (v!, lo!, hi!)
DECLARE FUNCTION RoadCenterX! (tPers!, laneX!, heading!, yawRate!, speed!, camLean!)

Randomize Timer

Screen _NewImage(SW, SH, 32)
_Title "Teenage Driving Simulator"

InitFonts

Do
    TitleScreen
    GameLoop
Loop

'-------------------------------
Sub InitFonts ()
    fontLarge& = _LoadFont("C:\Windows\Fonts\consola.ttf", 34)
    If fontLarge& = 0 Then fontLarge& = _LoadFont("C:\Windows\Fonts\lucon.ttf", 34)
    If fontLarge& = 0 Then fontLarge& = _LoadFont("C:\Windows\Fonts\cour.ttf", 34)

    fontSmall& = _LoadFont("C:\Windows\Fonts\consola.ttf", 16)
    If fontSmall& = 0 Then fontSmall& = _LoadFont("C:\Windows\Fonts\lucon.ttf", 16)
    If fontSmall& = 0 Then fontSmall& = _LoadFont("C:\Windows\Fonts\cour.ttf", 16)
End Sub

'-------------------------------
Sub TitleScreen ()
    Dim i%, x%, y%, kh%
    Dim colBright&, colDim&, colGlass&
    colBright& = _RGB32(0, 255, 0)
    colDim& = _RGBA32(0, 255, 0, 140)
    colGlass& = _RGBA32(0, 255, 0, 35)

    ' One-time init (stars + boot text)
    If titleInit% = 0 Then
        titleInit% = -1
        For i% = 1 To STARCOUNT
            starX(i%) = Rnd * (SW - 1)
            starY(i%) = Rnd * (SH - 1)
            starV(i%) = 30 + Rnd * 220
            starA(i%) = 25 + Int(Rnd * 90)
        Next

        bootLine(1) = "HIMEM is testing memory..."
        bootLine(2) = "Loading Remote Control Panel"
        bootLine(3) = "COM1: OK   LINK: STABLE"
        bootLine(4) = "CALIBRATING INPUT..."
        bootLine(5) = "TRACTION MODEL: QUESTIONABLE"
        bootLine(6) = "PEDESTRIANS: DETECTED"
        bootLine(7) = "READY."
    End If

    ' Reset typing timer every time we ENTER the title screen
    titleStart! = Timer

    Do
        Cls

        Dim now!, dt!, elapsed!
        now! = Timer
        elapsed! = now! - titleStart!
        If elapsed! < 0 Then elapsed! = elapsed! + 86400

        dt! = 1! / 60!

        ' Background
        Line (0, 0)-(SW - 1, SH - 1), _RGB32(0, 0, 0), BF

        ' Starfield
        For i% = 1 To STARCOUNT
            starY(i%) = starY(i%) + starV(i%) * dt!
            If starY(i%) > SH + 2 Then
                starY(i%) = -2
                starX(i%) = Rnd * (SW - 1)
                starV(i%) = 30 + Rnd * 220
                starA(i%) = 25 + Int(Rnd * 90)
            End If
            PSet (Int(starX(i%)), Int(starY(i%))), _RGBA32(0, 255, 0, starA(i%))
        Next

        ' Frames / panels
        Line (18, 18)-(SW - 19, SH - 19), colDim&, B
        Line (24, 24)-(SW - 25, SH - 25), _RGBA32(0, 255, 0, 35), B
        Line (38, 56)-(SW - 39, 120), colGlass&, BF
        Line (38, 56)-(SW - 39, 120), colDim&, B

        ' Title
        If fontLarge& <> 0 Then _Font fontLarge&
        Color colBright&, _RGB32(0, 0, 0)
        _PrintString (98, 70), "Teenage Driving Simulator"
        _PrintString (96, 68), "Teenage Driving Simulator"
        _PrintString (100, 72), "Teenage Driving Simulator"

        If fontSmall& <> 0 Then _Font fontSmall& Else _Font 0
        _PrintString (52, 150), "REMOTE DRIVER PANEL // CRT LINK: STABLE // SENSOR ARRAY: ONLINE"
        _PrintString (52, 172), "W/SPACE = GO    S/X = BRAKE    A/D = STEER    ESC = QUIT"

        ' Fake boot box
        Line (52, 208)-(SW - 53, 332), _RGBA32(0, 255, 0, 45), BF
        Line (52, 208)-(SW - 53, 332), colDim&, B

        Dim chars%, perSec!
        perSec! = 55
        chars% = Int(elapsed! * perSec!)

        Dim bx%, by%, n%, take%, s$
        bx% = 70: by% = 224
        For n% = 1 To BOOTLINES
            s$ = bootLine(n%)
            take% = Len(s$)
            If chars% <= 0 Then Exit For
            If chars% < take% Then take% = chars%
            _PrintString (bx%, by% + (n% - 1) * 14), Left$(s$, take%)
            chars% = chars% - Len(s$)
        Next

        ' Start prompt
        Dim blinkOn%
        blinkOn% = ((Int(elapsed! * 2) Mod 2) = 0)
        If blinkOn% Then
            Line (210, 370)-(590, 410), colGlass&, BF
            Line (210, 370)-(590, 410), colDim&, B
            _PrintString (252, 385), "PRESS ENTER TO START"
        End If

        ' Wheel preview
        DrawSteeringWheel (SW / 2), 500, 72, Sin(elapsed! * 2.2) * 65, colBright&

        ' CRT noise
        DrawScanlines
        For i% = 1 To 220
            x% = Int(Rnd * SW)
            y% = Int(Rnd * SH)
            PSet (x%, y%), _RGBA32(0, 255, 0, 20 + Int(Rnd * 55))
        Next

        _Display
        _Limit 60

        ' Input (clean exit)
        kh% = _KeyHit
        If kh% = 27 Then System
        If kh% = 13 Then Exit Do
    Loop
End Sub

'-------------------------------
Sub GameLoop ()
    Dim colBright&, colDim&, colDark&, colGlass&
    colBright& = _RGB32(0, 255, 0)
    colDim& = _RGBA32(0, 255, 0, 140)
    colDark& = _RGB32(0, 90, 0)
    colGlass& = _RGBA32(0, 255, 0, 35)

    Dim horizonY%, dashH%, roadY%, dashTop%
    horizonY% = 105
    dashH% = 220
    roadY% = SH - dashH% - 4
    dashTop% = roadY% + 1

    Dim speed!, fuel!, temp!, rpm!
    Dim steerIn!, steerAng!
    Dim yawRate!, heading!
    Dim laneX!, laneV!
    Dim collisions&, odom!
    Dim gear$

    Dim camLean!, camLeanTarget!
    Dim shake!, shakeT!
    Dim skidRisk!, skidWarn%
    Dim style&, nearFlash%
    Dim squealTimer%

    Dim t!, lastT!, dt!
    Dim keyFwd%, keyBrake%, keyLeft%, keyRight%

    Dim blink%, hitFlash%
    hitFlash% = 0
    blink% = 0

    ' Road draw helpers (DECLARE ONCE)
    Dim y%, tPers!, halfW!, centerX!
    Dim stripe%
    Static scroll!

    speed! = 0
    fuel! = 100
    temp! = 55
    rpm! = 0
    collisions& = 0
    odom! = 0
    gear$ = "D"

    steerAng! = 0
    yawRate! = 0
    heading! = 0
    laneX! = 0
    laneV! = 0

    camLean! = 0
    style& = 0
    nearFlash% = 0
    squealTimer% = 0

    Dim i%
    For i% = 1 To MAXP
        pedActive(i%) = 0
        pedScored(i%) = 0
    Next
    For i% = 1 To MAXP
        SpawnPed i%, 1700 + Rnd * 3600
    Next
    For i% = 1 To MAXT
        SpawnTree i%, 900 + Rnd * 4200
    Next

    lastT! = Timer

    Do
        t! = Timer
        dt! = t! - lastT!
        If dt! < 0 Then dt! = dt! + 86400
        lastT! = t!
        If dt! > .05 Then dt! = .05

        If _KeyDown(27) Then System

        blink% = (blink% + 1) Mod 30
        If hitFlash% > 0 Then hitFlash% = hitFlash% - 1
        If nearFlash% > 0 Then nearFlash% = nearFlash% - 1
        If squealTimer% > 0 Then squealTimer% = squealTimer% - 1

        keyFwd% = (_KeyDown(Asc("W")) Or _KeyDown(32))
        keyBrake% = (_KeyDown(Asc("S")) Or _KeyDown(Asc("X")))
        keyLeft% = _KeyDown(Asc("A"))
        keyRight% = _KeyDown(Asc("D"))

        steerIn! = 0
        If keyLeft% Then steerIn! = steerIn! - 1
        If keyRight% Then steerIn! = steerIn! + 1

        If fuel! > 0 Then
            If keyFwd% Then
                speed! = speed! + (320 + speed! * 0.35) * dt!
            End If
        End If
        If keyBrake% Then speed! = speed! - (520 + speed! * 0.25) * dt!

        speed! = speed! - (24 * dt!) - (speed! * 0.0115 * dt!)
        speed! = Clamp!(speed!, 0, 165)

        fuel! = fuel! - (speed! * 0.0031 * dt!)
        If fuel! < 0 Then fuel! = 0

        rpm! = speed! * 60
        If rpm! > 7600 Then rpm! = 7600

        temp! = temp! + (speed! / 165!) * 0.40 * dt! - 0.055 * dt!
        temp! = Clamp!(temp!, 45, 104)

        odom! = odom! + speed! * 0.012 * dt!

        ' Turning
        Dim steerRate!, steerReturn!
        steerRate! = 11.0 - (speed! / 30!)
        steerRate! = Clamp!(steerRate!, 5.0, 11.0)
        steerReturn! = 7.8

        If steerIn! = 0 Then
            steerAng! = steerAng! + (0 - steerAng!) * (steerReturn! * dt!)
        Else
            steerAng! = steerAng! + (steerIn! - steerAng!) * (steerRate! * dt!)
        End If
        steerAng! = Clamp!(steerAng!, -1.0, 1.0)

        Dim yawTarget!, yawResp!
        yawTarget! = steerAng! * (0.75 + speed! / 95!) * 2.7
        yawResp! = 8.5 + speed! / 32!
        yawRate! = yawRate! + (yawTarget! - yawRate!) * (yawResp! * dt!)
        yawRate! = Clamp!(yawRate!, -2.8, 2.8)

        heading! = heading! + yawRate! * dt!
        heading! = heading! * (1 - (0.16 * dt!))
        heading! = Clamp!(heading!, -3.4, 3.4)

        Dim latGain!, latDamp!
        latGain! = (0.52 + speed! / 220!) * 2.2
        latDamp! = 4.8 + speed! / 40!

        laneV! = laneV! + (steerAng! * latGain!) * dt!
        laneV! = laneV! * (1 - latDamp! * dt!)
        laneX! = laneX! + laneV! * dt!
        laneX! = laneX! * (1 - (0.22 * dt!))
        laneX! = Clamp!(laneX!, -1.15, 1.15)

        camLeanTarget! = Clamp!(yawRate! * 0.42 + steerAng! * 0.18, -1.0, 1.0)
        camLean! = camLean! + (camLeanTarget! - camLean!) * (6.8 * dt!)

        shakeT! = (speed! / 165!)
        shake! = (Rnd - 0.5) * 2.0 * (2 + 7 * shakeT!)

        skidRisk! = (Abs(yawRate!) * (speed! / 165!)) + (Abs(steerAng!) * (speed! / 165!) * 0.65)
        skidWarn% = (skidRisk! > 0.85 And speed! > 70 And (blink% < 15))

        If skidWarn% Then
            If squealTimer% = 0 Then
                Sound 900, .02
                squealTimer% = 6
            End If
        End If

        ' Update pedestrians
        Dim prevZ!, dz!, worldSpeed!
        worldSpeed! = speed! * 10.8

        For i% = 1 To MAXP
            If pedActive(i%) Then
                prevZ! = pedZ(i%)
                dz! = worldSpeed! * dt!
                pedZ(i%) = pedZ(i%) - dz!

                pedPhase(i%) = pedPhase(i%) + (0.8 + speed! / 240!) * dt!
                If pedPhase(i%) > 1000 Then pedPhase(i%) = pedPhase(i%) - 1000

                If pedZ(i%) < 520 Then
                    pedVX(i%) = pedVX(i%) + ((Rnd - 0.5) * 0.14) * dt!
                End If

                pedX(i%) = pedX(i%) + pedVX(i%) * dt!
                If pedX(i%) < -1.06 Then pedX(i%) = -1.06: pedVX(i%) = Abs(pedVX(i%))
                If pedX(i%) > 1.06 Then pedX(i%) = 1.06: pedVX(i%) = -Abs(pedVX(i%))

                If prevZ! > 75 And pedZ(i%) <= 75 Then
                    If Abs(pedX(i%) - laneX!) < 0.19 Then
                        collisions& = collisions& + 1
                        hitFlash% = 18
                        Sound 180, .07
                        SpawnPed i%, 1800 + Rnd * 3600
                    End If
                End If

                If prevZ! > 105 And pedZ(i%) <= 105 Then
                    If pedScored(i%) = 0 Then
                        Dim missD!
                        missD! = Abs(pedX(i%) - laneX!)
                        If missD! >= 0.19 And missD! < 0.30 Then
                            style& = style& + 25 + Int(speed! * 0.20)
                            nearFlash% = 18
                            pedScored(i%) = -1
                            Sound 700, .02
                        End If
                    End If
                End If

                If pedZ(i%) < 0 Then SpawnPed i%, 1800 + Rnd * 3600
            End If
        Next

        ' Update trees
        For i% = 1 To MAXT
            treeZ(i%) = treeZ(i%) - (speed! * 10.0 * dt!)
            If treeZ(i%) < 0 Then SpawnTree i%, 1200 + Rnd * 4400
        Next

        ' DRAW
        Cls
        If fontSmall& <> 0 Then _Font fontSmall& Else _Font 0
        Color colBright&, _RGB32(0, 0, 0)

        Line (0, 0)-(SW - 1, roadY%), _RGB32(0, 0, 0), BF

        If speed! > 90 Then
            Dim sN%, sx%, sy%, sl%
            For sN% = 1 To 22
                sx% = Int(Rnd * SW)
                sy% = Int(Rnd * roadY%)
                sl% = 10 + Int(Rnd * 22)
                Line (sx%, sy%)-(sx% - sl%, sy% + sl%), _RGBA32(0, 255, 0, 18)
            Next
        End If

        Line (0, 0)-(22, roadY%), _RGBA32(0, 255, 0, 20), BF
        Line (SW - 23, 0)-(SW - 1, roadY%), _RGBA32(0, 255, 0, 20), BF

        scroll! = scroll! + speed! * 16 * dt!

        Dim cxShake!
        cxShake! = shake!

        ' IMPORTANT: y%, tPers!, halfW!, centerX! already DIM'd above. Do NOT DIM again.

        For y% = horizonY% To roadY% Step 3
            tPers! = (y% - horizonY%) / (roadY% - horizonY%)
            If tPers! < 0 Then tPers! = 0
            If tPers! > 1 Then tPers! = 1

            halfW! = 150 + 540 * tPers!
            centerX! = RoadCenterX!(tPers!, laneX!, heading!, yawRate!, speed!, camLean!) + cxShake!

            Line (0, y%)-(centerX! - halfW! - 14, y%), _RGB32(0, 35, 0)
            Line (centerX! + halfW! + 14, y%)-(SW - 1, y%), _RGB32(0, 35, 0)

            Line (centerX! - halfW!, y%)-(centerX! + halfW!, y%), colDark&

            stripe% = Int(scroll! + tPers! * 640) Mod 60
            If stripe% < 16 Then Line (centerX!, y%)-(centerX!, y% + 2), colBright&

            Line (centerX! - halfW! - 6, y%)-(centerX! - halfW! - 6, y% + 2), colDim&
            Line (centerX! + halfW! + 6, y%)-(centerX! + halfW! + 6, y% + 2), colDim&
        Next

        ' Trees
        Dim f!, py!, px!
        For i% = 1 To MAXT
            f! = 1 / (1 + treeZ(i%) / 360!)
            py! = horizonY% + (roadY% - horizonY%) * f!
            If py! > horizonY% And py! < roadY% Then
                halfW! = 150 + 540 * f!
                centerX! = RoadCenterX!(f!, laneX!, heading!, yawRate!, speed!, camLean!) + cxShake!
                px! = centerX! + treeSide(i%) * (halfW! + 60 + treeVar(i%) * 26)

                Dim trunkH!, trunkW!, crownR!
                trunkH! = 22 + 120 * f!
                trunkW! = 4 + 16 * f!
                crownR! = 10 + 55 * f!

                Line (px! - trunkW! / 2, py! - trunkH!)-(px! + trunkW! / 2, py!), _RGBA32(0, 255, 0, 150), BF
                Circle (px!, py! - trunkH! - crownR! * 0.45), crownR!, _RGBA32(0, 255, 0, 160)
                Circle (px! - crownR! * 0.55, py! - trunkH! - crownR! * 0.15), crownR! * 0.85, _RGBA32(0, 255, 0, 130)
                Circle (px! + crownR! * 0.55, py! - trunkH! - crownR! * 0.15), crownR! * 0.85, _RGBA32(0, 255, 0, 130)
            End If
        Next

        ' Peds
        For i% = 1 To MAXP
            If pedActive(i%) Then
                f! = 1 / (1 + pedZ(i%) / 320!)
                py! = horizonY% + (roadY% - horizonY%) * f!
                halfW! = 150 + 540 * f!
                centerX! = RoadCenterX!(f!, laneX!, heading!, yawRate!, speed!, camLean!) + cxShake!
                px! = centerX! + pedX(i%) * halfW! * 0.92

                If py! > horizonY% And py! < roadY% Then
                    DrawPed px!, py!, f!, pedType(i%), pedPhase(i%), colBright&, colDim&
                End If
            End If
        Next

        Dim sigL%, sigR%, warnLowFuel%, warnHot%, warnOver%
        sigL% = (keyLeft% And (blink% < 15))
        sigR% = (keyRight% And (blink% < 15))
        warnLowFuel% = (fuel! < 15 And (blink% < 15))
        warnHot% = (temp! > 92 And (blink% < 15))
        warnOver% = (speed! > 125 And (blink% < 15))

        DrawHUD dashTop%, speed!, rpm!, fuel!, temp!, gear$, collisions&, odom!, steerAng!, laneX!, yawRate!, style&, sigL%, sigR%, warnLowFuel%, warnHot%, warnOver%, skidWarn%, colBright&, colDim&, colGlass&

        If nearFlash% > 0 Then
            Line (0, 0)-(SW - 1, roadY%), _RGBA32(0, 255, 0, 16), BF
            _PrintString (18, 18), "NEAR MISS +STYLE"
        End If

        If hitFlash% > 0 Then
            Line (0, 0)-(SW - 1, roadY%), _RGBA32(0, 0, 0, 70), BF
            _PrintString (330, 20), "COLLISION"
        End If

        DrawScanlines
        Dim n%, rx%, ry%
        For n% = 1 To 110
            rx% = Int(Rnd * SW)
            ry% = Int(Rnd * (roadY% + 1))
            PSet (rx%, ry%), _RGBA32(0, 255, 0, 40)
        Next

        _Display
        _Limit 60
    Loop
End Sub

'-------------------------------
Function RoadCenterX! (tPers!, laneX!, heading!, yawRate!, speed!, camLean!)
    Dim s!, curveStrength!, nearStrength!, far!, near!, laneOffset!, leanOffset!

    s! = 1 - tPers!
    If s! < 0 Then s! = 0
    If s! > 1 Then s! = 1

    curveStrength! = 250 + speed! * 1.05
    nearStrength! = 150 + speed! * 0.75

    far! = heading! * curveStrength!
    near! = yawRate! * nearStrength!

    laneOffset! = -laneX! * (90 + 380 * tPers!)
    leanOffset! = camLean! * (18 + 26 * (1 - tPers!))

    RoadCenterX! = (SW / 2) + far! * (s! * s! * s!) + near! * (s! * s!) + laneOffset! + leanOffset!
End Function

'-------------------------------
Sub DrawHUD (dashTop%, speed!, rpm!, fuel!, temp!, gear$, collisions&, odom!, steerAng!, laneX!, yawRate!, style&, sigL%, sigR%, warnLowFuel%, warnHot%, warnOver%, skidWarn%, colBright&, colDim&, colGlass&)
    Dim baseY%, pTop%, pBot%
    baseY% = dashTop%
    pTop% = baseY% + 52
    pBot% = SH - 12

    Line (0, baseY%)-(SW - 1, SH - 1), _RGB32(0, 0, 0), BF
    Line (0, baseY%)-(SW - 1, SH - 1), colBright&, B

    Dim g%, hDash%
    hDash% = SH - baseY%
    For g% = 0 To hDash% - 1
        Line (0, baseY% + g%)-(SW - 1, baseY% + g%), _RGBA32(0, 255, 0, 2 + (g% \ 3))
    Next

    Line (10, baseY% + 10)-(SW - 11, baseY% + 42), colGlass&, BF
    Line (10, baseY% + 10)-(SW - 11, baseY% + 42), colDim&, B

    If fontSmall& <> 0 Then _Font fontSmall& Else _Font 0
    _PrintString (18, baseY% + 18), "REMOTE DRIVER PANEL"
    _PrintString (18, baseY% + 32), "CTRL: W/SPACE GO  S/X BRAKE  A/D STEER"

    If sigL% Then _PrintString (SW \ 2 - 46, baseY% + 18), "<-"
    If sigR% Then _PrintString (SW \ 2 + 18, baseY% + 18), "->"

    Dim wx%
    wx% = SW - 300
    If warnLowFuel% Then _PrintString (wx%, baseY% + 18), "LOW FUEL"
    If warnHot% Then _PrintString (wx% + 88, baseY% + 18), "ENGINE HOT"
    If warnOver% Then _PrintString (wx% + 184, baseY% + 18), "OVERSPEED"
    If skidWarn% Then _PrintString (wx% + 280, baseY% + 18), "SKID!"

    Line (14, pTop%)-(268, pBot%), colDim&, B
    Line (276, pTop%)-(544, pBot%), colDim&, B
    Line (552, pTop%)-(786, pBot%), colDim&, B

    _PrintString (24, pTop% + 10), "SPEED"
    Dim spdStr$
    spdStr$ = Right$("000" + LTrim$(Str$(Int(speed! + 0.5))), 3)

    If fontLarge& <> 0 Then _Font fontLarge& Else _Font 0
    _PrintString (28, pTop% + 22), spdStr$
    If fontSmall& <> 0 Then _Font fontSmall& Else _Font 0
    _PrintString (172, pTop% + 38), "MPH"
    _PrintString (28, pTop% + 70), "GEAR: " + gear$

    Dim tRpm!, tFuel!, tTemp!
    tRpm! = rpm! / 7600!
    tFuel! = fuel! / 100!
    tTemp! = (temp! - 45!) / 59!

    _PrintString (28, pTop% + 96), "RPM"
    DrawBarH 72, pTop% + 92, 182, 14, tRpm!, colBright&, colDim&, colGlass&
    _PrintString (28, pTop% + 122), "FUEL"
    DrawBarH 72, pTop% + 118, 182, 14, tFuel!, colBright&, colDim&, colGlass&
    _PrintString (28, pTop% + 148), "TEMP"
    DrawBarH 72, pTop% + 144, 182, 14, tTemp!, colBright&, colDim&, colGlass&
    _PrintString (28, pTop% + 176), "HITS: " + LTrim$(Str$(collisions&))

    _PrintString (286, pTop% + 10), "LANE POSITION"
    Dim laneT!
    laneT! = (laneX! + 1.15) / 2.3
    laneT! = Clamp!(laneT!, 0, 1)

    Dim gx1%, gx2%, gy%, mx%, cx%
    gx1% = 292
    gx2% = 528
    gy% = pTop% + 54
    Line (gx1%, gy%)-(gx2%, gy%), colDim&
    cx% = (gx1% + gx2%) \ 2
    Line (cx%, gy% - 6)-(cx%, gy% + 6), colBright&
    mx% = gx1% + Int((gx2% - gx1%) * laneT!)
    Circle (mx%, gy%), 4, colBright&
    Line (mx% - 6, gy% + 8)-(mx% + 6, gy% + 8), colDim&

    _PrintString (286, pTop% + 78), "SLIP"
    Dim slipT!
    slipT! = Abs(yawRate!) / 2.8
    slipT! = Clamp!(slipT!, 0, 1)
    DrawBarV 492, pTop% + 74, 36, 98, slipT!, colBright&, colDim&, colGlass&

    DrawSteeringWheel 410, pTop% + 162, 78, steerAng! * 95, colBright&

    _PrintString (286, pTop% + 198), "STEER"
    Dim steerT!
    steerT! = (steerAng! + 1) / 2
    steerT! = Clamp!(steerT!, 0, 1)
    DrawBarH 344, pTop% + 194, 138, 14, steerT!, colBright&, colDim&, colGlass&

    _PrintString (562, pTop% + 10), "TELEMETRY"
    _PrintString (562, pTop% + 34), "SPD: " + Right$("   " + LTrim$(Str$(Int(speed! + 0.5))), 3) + " MPH"
    _PrintString (562, pTop% + 52), "RPM: " + Right$("0000" + LTrim$(Str$(Int(rpm!))), 4)
    _PrintString (562, pTop% + 70), "FUEL: " + Right$("000" + LTrim$(Str$(Int(fuel! + 0.5))), 3) + "%"
    _PrintString (562, pTop% + 88), "TEMP: " + Right$("000" + LTrim$(Str$(Int(temp! + 0.5))), 3) + "C"
    _PrintString (562, pTop% + 116), "STYLE: " + LTrim$(Str$(style&))

    Dim odomStr$
    odomStr$ = Right$("000000" + LTrim$(Str$(Int(odom! * 1000))), 6)
    _PrintString (562, pTop% + 134), "ODOM: " + odomStr$

    Line (562, pTop% + 160)-(774, pTop% + 196), colGlass&, BF
    Line (562, pTop% + 160)-(774, pTop% + 196), colDim&, B
    _PrintString (570, pTop% + 168), "SYS: OK"
    _PrintString (570, pTop% + 184), "LINK: STABLE"

    Dim yy%
    For yy% = baseY% To SH - 1 Step 4
        Line (0, yy%)-(SW - 1, yy%), _RGBA32(0, 0, 0, 70)
    Next
End Sub

'-------------------------------
Sub DrawBarH (x%, y%, w%, h%, t!, colBright&, colDim&, colGlass&)
    Dim fillW%, i%
    If t! < 0 Then t! = 0
    If t! > 1 Then t! = 1

    Line (x%, y%)-(x% + w%, y% + h%), colDim&, B

    fillW% = Int((w% - 2) * t!)
    If fillW% > 0 Then
        Line (x% + 1, y% + 1)-(x% + 1 + fillW%, y% + h% - 1), colGlass&, BF
        For i% = 0 To h% - 2 Step 2
            Line (x% + 1, y% + 1 + i%)-(x% + 1 + fillW%, y% + 1 + i%), colBright&
        Next
    End If
End Sub

'-------------------------------
Sub DrawBarV (x%, y%, w%, h%, t!, colBright&, colDim&, colGlass&)
    Dim fillH%, yFill%, i%
    If t! < 0 Then t! = 0
    If t! > 1 Then t! = 1

    Line (x%, y%)-(x% + w%, y% + h%), colDim&, B

    fillH% = Int((h% - 2) * t!)
    yFill% = y% + h% - 1 - fillH%
    If fillH% > 0 Then
        Line (x% + 1, yFill%)-(x% + w% - 1, y% + h% - 1), colGlass&, BF
        For i% = 0 To w% - 2 Step 2
            Line (x% + 1 + i%, yFill%)-(x% + 1 + i%, y% + h% - 1), colBright&
        Next
    End If
End Sub

'-------------------------------
Sub DrawSteeringWheel (cx!, cy!, r!, angDeg!, col&)
    Dim a!, i%, sa!
    Dim x1!, y1!, x2!, y2!

    a! = angDeg! * 3.14159265 / 180

    Circle (cx!, cy!), r!, col&
    Circle (cx!, cy!), r! - 10, _RGBA32(0, 255, 0, 120)

    Circle (cx!, cy!), 18, col&
    Paint (cx!, cy!), _RGBA32(0, 255, 0, 25), col&

    For i% = 0 To 2
        sa! = a! + (i% * 2.0943951)
        x1! = cx! + Cos(sa!) * 26
        y1! = cy! + Sin(sa!) * 26
        x2! = cx! + Cos(sa!) * (r! - 16)
        y2! = cy! + Sin(sa!) * (r! - 16)
        Line (x1!, y1!)-(x2!, y2!), col&
        Line (x1! + 1, y1! + 1)-(x2! + 1, y2! + 1), _RGBA32(0, 255, 0, 80)
    Next
End Sub

'-------------------------------
Sub DrawPed (px!, py!, f!, pType%, phase!, colBright&, colDim&)
    Dim h!, w!, headR!
    Dim gait!, bob!, arm!, leg!

    Select Case pType%
        Case 1
            h! = 44 * f!: w! = 10 * f!: headR! = 5 * f!
        Case 2
            h! = 60 * f!: w! = 16 * f!: headR! = 6 * f!
        Case Else
            h! = 52 * f!: w! = 12 * f!: headR! = 6 * f!
    End Select

    gait! = Sin(phase! * 6.28318)
    bob! = Abs(gait!) * (2.2 * f!)
    arm! = (6 + 8 * f!) * (-gait!)
    leg! = (7 + 10 * f!) * (gait!)

    Line (px! - w! / 2, py! - h! - bob!)-(px! + w! / 2, py! - bob!), colDim&, BF
    Line (px! - w! / 2, py! - h! - bob!)-(px! + w! / 2, py! - bob!), colBright&, B

    Circle (px!, py! - h! - bob! - headR! * 1.5), headR!, colBright&
    Paint (px!, py! - h! - bob! - headR! * 1.5), _RGBA32(0, 255, 0, 35), colBright&

    Line (px! - w! * 0.25, py! - bob!)-(px! - w! * 0.25 + leg!, py! + 10 * f!), colBright&
    Line (px! + w! * 0.25, py! - bob!)-(px! + w! * 0.25 - leg!, py! + 10 * f!), colBright&

    Line (px! - w! / 2, py! - h! * 0.65 - bob!)-(px! - w! / 2 + arm!, py! - h! * 0.45 - bob!), colBright&
    Line (px! + w! / 2, py! - h! * 0.65 - bob!)-(px! + w! / 2 - arm!, py! - h! * 0.45 - bob!), colBright&
End Sub

'-------------------------------
Sub DrawScanlines ()
    Dim y%
    For y% = 0 To SH - 1 Step 2
        Line (0, y%)-(SW - 1, y%), _RGBA32(0, 0, 0, 90)
    Next
End Sub

'-------------------------------
Sub SpawnPed (i%, startZ!)
    pedActive(i%) = -1
    pedZ(i%) = startZ!
    pedX(i%) = (Rnd * 2.0) - 1.0
    pedVX(i%) = (Rnd * 0.7) - 0.35
    If Abs(pedVX(i%)) < 0.06 Then pedVX(i%) = Sgn(pedVX(i%) + .001) * 0.12

    pedType(i%) = Int(Rnd * 3)
    pedPhase(i%) = Rnd
    pedScored(i%) = 0
End Sub

'-------------------------------
Sub SpawnTree (i%, startZ!)
    treeZ(i%) = startZ!
    If Rnd < 0.5 Then treeSide(i%) = -1 Else treeSide(i%) = 1
    treeVar(i%) = (Rnd * 2) - 1
End Sub

'-------------------------------
Function Clamp! (v!, lo!, hi!)
    If v! < lo! Then v! = lo!
    If v! > hi! Then v! = hi!
    Clamp! = v!
End Function

