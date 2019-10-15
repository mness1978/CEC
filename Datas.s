;; Data
;; internals
ConstsList:
    DC.L    0
;*-*

;; LibCalls


; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   The Internal LIBRARYCALLS               ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;


EXEC:
 DC.B "Supervisor",13,16,16,16,16,16,16,"InitCode",0
 DC.B 1,"InitStruct",9,10,0,"MakeLibrary",8,9,10,0,1,"MakeFunctions",8,9,10
 DC.B "FindResident",9,"InitResident",9,1,"Alert",7,"Debug",0,"Disable",16
 DC.B "Enable",16,"Forbid",16,"Permit",16,"SetSR",0,1,"SuperState",16
 DC.B "UserState",0,"SetIntVector",0,9,"AddIntServer",0,9,"RemIntServer",0,9
 DC.B "Cause",9,"Allocate",8,0,"Deallocate",8,9,0,"AllocMem",0,1,"AllocAbs",0
 DC.B 9,"FreeMem",9,0,"AvailMem",1,"AllocEntry",8,"FreeEntry",8,"Insert",8,9
 DC.B 10,"AddHead",8,9,"AddTail",8,9,"Remove",9,"RemHead",8,"RemTail",8
 DC.B "Enqueue",8,9,"FindName",8,9,"AddTask",9,10,11,"RemTask",9,"FindTask",9
 DC.B "SetTaskPri",9,0,"SetSignal",0,1,"SetExcept",0,1,"Wait",0,"Signal",9,0
 DC.B "AllocSignal",0,"FreeSignal",0,"AllocTrap",0,"FreeTrap",0,"AddPort",9
 DC.B "RemPort",9,"PutMsg",8,9,"GetMsg",8,"ReplyMsg",9,"WaitPort",8,"FindPort"
 DC.B 9,"AddLibrary",9,"RemLibrary",9,"OldOpenLibrary",9,"CloseLibrary",9
 DC.B "SetFunction",9,8,0,"SumLibrary",9,"AddDevice",9,"RemDevice",9
 DC.B "OpenDevice",8,0,9,1,"CloseDevice",9,"DoIO",9,"SendIO",9,"CheckIO",9
 DC.B "WaitIO",9,"AbortIO",9,"AddResource",9,"RemResource",9,"OpenResource",9
 DC.B 16,16,16,"RawDoFmt",8,9,10,11,"GetCC",16,"TypeOfMem",9,"Procure",8,9
 DC.B "Vacate",8,9,"OpenLibrary",9,0,"InitSemaphore",8,"ObtainSemaphore",8
 DC.B "ReleaseSemaphore",8,"AttemptSemaphore",8,"ObtainSemaphoreList",8
 DC.B "ReleaseSemaphoreList",8,"FindSemaphore",9,"AddSemaphore",9
 DC.B "RemSemaphore",9,"SumKickData",16,"AddMemList",0,1,2,8,9,"CopyMem",8,9,0
 DC.B "CopyMemQuick",8,9,0,"CacheClearU",16,"CacheClearE",8,0,1,"CacheControl"
 DC.B 0,1,"CreateIORequest",8,0,"DeleteIORequest",8,"CreateMsgPort",16
 DC.B "DeleteMsgPort",8,"ObtainSemaphoreShared",8,"AllocVec",0,1,"FreeVec",9
 DC.B "CreatePool",0,1,2,"DeletePool",8,"AllocPooled",8,0,"FreePooled",8,9,0
 DC.B "AttemptSemaphoreShared",8,"ColdReboot",16,"StackSwap",8,"ChildFree",0
 DC.B "ChildOrphan",0,"ChildStatus",0,"ChildWait",0,"CachePreDMA",8,9,0
 DC.B "CachePostDMA",8,9,0,"AddMemHandler",9,"RemMemHandler",9
 DC.B "ObtainQuickVector",8,"ÿ",0,0,0,0

GFX:
 DC.B "BltBitMap",8,0,1,9,2,3,4,5,6,7,10
 DC.B "BltTemplate",8,0,1,9,2,3,4,5,"ClearEOL",9,"ClearScreen",9,"TextLength"
 DC.B 9,8,0,"Text",9,8,0,"SetFont",9,8,"OpenFont",8,"CloseFont",9
 DC.B "AskSoftStyle",9,"SetSoftStyle",9,0,1,"AddBob",8,9,"AddVSprite",8,9
 DC.B "DoCollision",9,"DrawGList",9,8,"InitGels",8,9,10,"InitMasks",8
 DC.B "RemIBob",8,9,10,"RemVSprite",8,"SetCollision",0,8,9,"SortGList",9
 DC.B "AddAnimOb",8,9,10,"Animate",8,9,"GetGBuffers",8,9,0,"InitGMasks",8
 DC.B "DrawEllipse",9,0,1,2,3,"AreaEllipse",9,0,1,2,3,"LoadRGB4",8,9,0
 DC.B "InitRastPort",9,"InitVPort",8,"MrgCop",9,"MakeVPort",8,9,"LoadView",9
 DC.B "WaitBlit",16,"SetRast",9,0,"Move",9,0,1,"Draw",9,0,1,"AreaMove",9,0,1
 DC.B "AreaDraw",9,0,1,"AreaEnd",9,"WaitTOF",16,"Qblit",9,"InitArea",8,9,0
 DC.B "SetRGB4",8,0,1,2,3,"QbSBlit",9,"BltClear",9,0,1,"RectFill",9,0,1,2,3
 DC.B "BltPattern",9,8,0,1,2,3,4,"ReadPixel",9,0,1,"WritePixel",9,0,1,"Flood"
 DC.B 9,2,0,1,"PolyDraw",9,0,8,"SetAPen",9,0,"SetBPen",9,0,"SetDrMd",9,0
 DC.B "InitView",9,"Cbump",9,"Cmove",9,0,1,"Cwait",9,0,1,"VbeamPos",16
 DC.B "InitBitMap",8,0,1,2,"ScrollRaster",9,0,1,2,3,4,5,"WaitBOVP",8
 DC.B "GetSprite",8,0,"FreeSprite",0,"ChangeSprite",8,9,10,"MoveSprite",8,9,0
 DC.B 1,"LockLayerRom",13,"UnlockLayerRom",13,"SyncSBitMap",8,"CopySBitMap",8
 DC.B "OwnBlitter",16,"DisownBlitter",16,"InitTmpRas",8,9,0,"AskFont",9,8
 DC.B "AddFont",9,"RemFont",9,"AllocRaster",0,1,"FreeRaster",8,0,1
 DC.B "AndRectRegion",8,9,"OrRectRegion",8,9,"NewRegion",16,"ClearRectRegion"
 DC.B 8,9,"ClearRegion",8,"DisposeRegion",8,"FreeVPortCopLists",8
 DC.B "FreeCopList",8,"ClipBlit",8,0,1,9,2,3,4,5,6,"XorRectRegion",8,9
 DC.B "FreeCprList",8,"GetColorMap",0,"FreeColorMap",8,"GetRGB4",8,0
 DC.B "ScrollVPort",8,"UcopperListInit",8,0,"FreeGBuffers",8,9,0
 DC.B "BltBitMapRastPort",8,0,1,9,2,3,4,5,6,"OrRegionRegion",8,9
 DC.B "XorRegionRegion",8,9,"AndRegionRegion",8,9,"SetRGB4CM",8,0,1,2,3
 DC.B "BltMaskBitMapRastPort",8,0,1,9,2,3,4,5,6,10,16,16,"AttemptLockLayerRom"
 DC.B 13,"GfxNew",0,"GfxFree",8,"GfxAssociate",8,9,"BitMapScale",8,"ScalerDiv"
 DC.B 0,1,2,"TextExtent",9,8,0,10,"TextFit",9,8,0,10,11,1,2,3,"GfxLookUp",8
 DC.B "VideoControl",8,9,"OpenMonitor",9,0,"CloseMonitor",8,"FindDisplayInfo"
 DC.B 0,"NextDisplayInfo",0,16,16,16,"GetDisplayInfoData",8,9,0,1,2
 DC.B "FontExtent",8,9,"ReadPixelLine8",8,0,1,2,10,9,"WritePixelLine8",8,0,1,2
 DC.B 10,9,"ReadPixelArray8",8,0,1,2,3,10,9,"WritePixelArray8",8,0,1,2,3,10,9
 DC.B "GetVPModeID",8,"ModeNotAvailable",0,"WeighTAMatch",8,9,10,"EraseRect",9
 DC.B 0,1,2,3,"ExtendFont",8,9,"StripFont",8,"CalcIVG",8,9,"AttachPalExtra",8
 DC.B 9,"ObtainBestPenA",8,1,2,3,9,16,"SetRGB32",8,0,1,2,3,"GetAPen",8
 DC.B "GetBPen",8,"GetDrMd",8,"GetOutlinePen",8,"LoadRGB32",8,9,"SetChipRev",0
 DC.B "SetABPenDrMd",9,0,1,2,"GetRGB32",8,0,1,9,16,16,"AllocBitMap",0,1,2,3,8
 DC.B "FreeBitMap",8,"GetExtSpriteA",10,9,"CoerceMode",8,0,1,"ChangeVPBitMap"
 DC.B 8,9,10,"ReleasePen",8,0,"ObtainPen",8,0,1,2,3,4,"GetBitMapAttr",8,1
 DC.B "AllocDBufInfo",8,"FreeDBufInfo",9,"SetOutlinePen",8,0,"SetWriteMask",8
 DC.B 0,"SetMaxPen",8,0,"SetRGB32CM",8,0,1,2,3,"ScrollRasterBF",9,0,1,2,3,4,5
 DC.B "FindColor",11,1,2,3,4,16,"AllocSpriteDataA",10,9,"ChangeExtSpriteA",8,9
 DC.B 10,11,"FreeSpriteData",10,"SetRPAttrsA",8,9,"GetRPAttrsA",8,9
 DC.B "BestModeIDA",8,"WriteChunkyPixels",8,0,1,2,3,10,4,"ÿ",0,0,0,0


INTUI:
 DC.B "OpenIntuition",16,"Intuition",8
 DC.B "AddGadget",8,9,0,"ClearDMRequest",8,"ClearMenuStrip",8,"ClearPointer",8
 DC.B "CloseScreen",8,"CloseWindow",8,"CloseWorkBench",16,"CurrentTime",8,9
 DC.B "DisplayAlert",0,8,1,"DisplayBeep",8,"DoubleClick",0,1,2,3,"DrawBorder"
 DC.B 8,9,0,1,"DrawImage",8,9,0,1,"EndRequest",8,9,"GetDefPrefs",8,0
 DC.B "GetPrefs",8,0,"InitRequester",8,"ItemAddress",8,0,"ModifyIDCMP",8,0
 DC.B "ModifyProp",8,9,10,0,1,2,3,4,"MoveScreen",8,0,1,"MoveWindow",8,0,1
 DC.B "OffGadget",8,9,10,"OffMenu",8,0,"OnGadget",8,9,10,"OnMenu",8,0
 DC.B "OpenScreen",8,"OpenWindow",8,"OpenWorkBench",16,"PrintIText",8,9,0,1
 DC.B "RefreshGadgets",8,9,10,"RemoveGadget",8,9,"ReportMouse",0,8,"Request",8
 DC.B 9,"ScreenToBack",8,"ScreenToFront",8,"SetDMRequest",8,9,"SetMenuStrip",8
 DC.B 9,"SetPointer",8,9,0,1,2,3,"SetWindowTitles",8,9,10,"ShowTitle",8,0
 DC.B "SizeWindow",8,0,1,"ViewAddress",16,"ViewPortAddress",8,"WindowToBack",8
 DC.B "WindowToFront",8,"WindowLimits",8,0,1,2,3,"SetPrefs",8,0,1
 DC.B "IntuiTextLength",8,"WbenchToBack",16,"WbenchToFront",16,"AutoRequest",8
 DC.B 9,10,11,0,1,2,3,"BeginRefresh",8,"BuildSysRequest",8,9,10,11,0,1,2
 DC.B "EndRefresh",8,0,"FreeSysRequest",8,"MakeScreen",8,"RemakeDisplay",16
 DC.B "RethinkDisplay",16,"AllocRemember",8,0,1,"AlohaWorkbench",8
 DC.B "FreeRemember",8,0,"LockIBase",0,"UnlockIBase",8,"GetScreenData",8,0,1,9
 DC.B "RefreshGList",8,9,10,0,"AddGList",8,9,0,1,10,"RemoveGList",8,9,0
 DC.B "ActivateWindow",8,"RefreshWindowFrame",8,"ActivateGadget",8,9,10
 DC.B "NewModifyProp",8,9,10,0,1,2,3,4,5,"QueryOverscan",8,9,0
 DC.B "MoveWindowInFrontOf",8,9,"ChangeWindowBox",8,0,1,2,3,"SetEditHook",8
 DC.B "SetMouseQueue",8,0,"ZipWindow",8,"LockPubScreen",8,"UnlockPubScreen",8
 DC.B 9,"LockPubScreenList",16,"UnlockPubScreenList",16,"NextPubScreen",8,9
 DC.B "SetDefaultPubScreen",8,"SetPubScreenModes",0,"PubScreenStatus",8,0
 DC.B "ObtainGIRPort",8,"ReleaseGIRPort",8,"GadgetMouse",8,9,10,16
 DC.B "GetDefaultPubScreen",8,"EasyRequestArgs",8,9,10,11
 DC.B "BuildEasyRequestArgs",8,9,0,11,"SysReqHandler",8,9,0
 DC.B "OpenWindowTagList",8,9,"OpenScreenTagList",8,9,"DrawImageState",8,9,0,1
 DC.B 2,10,"PointInImage",0,8,"EraseImage",8,9,0,1,"NewObjectA",8,9,10
 DC.B "DisposeObject",8,"SetAttrsA",8,9,"GetAttr",0,8,9,"SetGadgetAttrsA",8,9
 DC.B 10,11,"NextObject",8,16,"MakeClass",8,9,10,0,1,"AddClass",8
 DC.B "GetScreenDrawInfo",8,"FreeScreenDrawInfo",8,9,"ResetMenuStrip",8,9
 DC.B "RemoveClass",8,"FreeClass",8,16,16,16,16,16,16,16,16
 DC.B "AllocScreenBuffer",8,9,0,"FreeScreenBuffer",8,9,"ChangeScreenBuffer",8
 DC.B 9,"ScreenDepth",8,0,9,"ScreenPosition",8,0,1,2,3,4,"ScrollWindowRaster"
 DC.B 9,0,1,2,3,4,5,"LendMenus",8,9,"DoGadgetMethodA",8,9,10,11
 DC.B "SetWindowPointerA",8,9,"TimedDisplayAlert",0,8,1,9,"HelpControl",8,0
 DC.B "ÿ",0,0,0,0

DOS:
 DC.B "Open",1,2,"Close",1,"Read",1,2,3,"Write",1,2,3
 DC.B "Input",16,"Output",16,"Seek",1,2,3,"DeleteFile",1,"Rename",1,2,"Lock",1
 DC.B 2,"UnLock",1,"DupLock",1,"Examine",1,2,"ExNext",1,2,"Info",1,2
 DC.B "CreateDir",1,"CurrentDir",1,"IoErr",16,"CreateProc",1,2,3,4,"Exit",1
 DC.B "LoadSeg",1,"UnLoadSeg",1,16,16,"DeviceProc",1,"SetComment",1,2
 DC.B "SetProtection",1,2,"DateStamp",1,"Delay",1,"WaitForChar",1,2
 DC.B "ParentDir",1,"IsInteractive",1,"Execute",1,2,3,"AllocDosObject",1,2
 DC.B "FreeDosObject",1,2,"DoPkt",1,2,3,4,5,6,7,"SendPkt",1,2,3,"WaitPkt",16
 DC.B "ReplyPkt",1,2,3,"AbortPkt",1,2,"LockRecord",1,2,3,4,5,"LockRecords",1,2
 DC.B "UnLockRecord",1,2,3,"UnLockRecords",1,"SelectInput",1,"SelectOutput",1
 DC.B "FgetC",1,"FputC",1,2,"UnGetC",1,2,"Fread",1,2,3,4,"Fwrite",1,2,3,4
 DC.B "Fgets",1,2,3,"Fputs",1,2,"VfWritef",1,2,3,"VfPrintf",1,2,3,"Flush",1
 DC.B "SetVBuf",1,2,3,4,"DupLockFromFH",1,"OpenFromLock",1,"ParentOfFH",1
 DC.B "ExamineFH",1,2,"SetFileDate",1,2,"NameFromLock",1,2,3,"NameFromFH",1,2
 DC.B 3,"SplitName",1,2,3,4,5,"SameLock",1,2,"SetMode",1,2,"ExAll",1,2,3,4,5
 DC.B "ReadLink",1,2,3,4,5,"MakeLink",1,2,3,"ChangeMode",1,2,3,"SetFileSize",1
 DC.B 2,3,"SetIoErr",1,"Fault",1,2,3,4,"PrintFault",1,2,"ErrorReport",1,2,3,4
 DC.B 16,"Cli",16,"CreateNewProc",1,"RunCommand",1,2,3,4,"GetConsoleTask",16
 DC.B "SetConsoleTask",1,"GetFileSysTask",16,"SetFileSysTask",1,"GetArgStr",16
 DC.B "SetArgStr",1,"FindCliProc",1,"MaxCli",16,"SetCurrentDirName",1
 DC.B "GetCurrentDirName",1,2,"SetProgramName",1,"GetProgramName",1,2
 DC.B "SetPrompt",1,"GetPrompt",1,2,"SetProgramDir",1,"GetProgramDir",16
 DC.B "SystemTagList",1,2,"AssignLock",1,2,"AssignLate",1,2,"AssignPath",1,2
 DC.B "AssignAdd",1,2,"RemAssignList",1,2,"GetDeviceProc",1,2,"FreeDeviceProc"
 DC.B 1,"LockDosList",1,"UnLockDosList",1,"AttemptLockDosList",1,"RemDosEntry"
 DC.B 1,"AddDosEntry",1,"FindDosEntry",1,2,3,"NextDosEntry",1,2,"MakeDosEntry"
 DC.B 1,2,"FreeDosEntry",1,"IsFileSystem",1,"Format",1,2,3,"Relabel",1,2
 DC.B "Inhibit",1,2,"AddBuffers",1,2,"CompareDates",1,2,"DateToStr",1
 DC.B "StrToDate",1,"InternalLoadSeg",0,8,9,10,"InternalUnLoadSeg",1,9
 DC.B "NewLoadSeg",1,2,"AddSegment",1,2,3,"FindSegment",1,2,3,"RemSegment",1
 DC.B "CheckSignal",1,"ReadArgs",1,2,3,"FindArg",1,2,"ReadItem",1,2,3
 DC.B "StrToLong",1,2,"MatchFirst",1,2,"MatchNext",1,"MatchEnd",1
 DC.B "ParsePattern",1,2,3,"MatchPattern",1,2,16,"FreeArgs",1,16,"FilePart",1
 DC.B "PathPart",1,"AddPart",1,2,3,"StartNotify",1,"EndNotify",1,"SetVar",1,2
 DC.B 3,4,"GetVar",1,2,3,4,"DeleteVar",1,2,"FindVar",1,2,16,"CliInitNewcli",8
 DC.B "CliInitRun",8,"WriteChars",1,2,"PutStr",1,"Vprintf",1,2,16
 DC.B "ParsePatternNoCase",1,2,3,"MatchPatternNoCase",1,2,16,"SameDevice",1,2
 DC.B "ExAllEnd",1,2,3,4,5,"SetOwner",1,2,"ÿ",0,0,0,0

UTIL:
 DC.B "FindTagItem",0,8,"GetTagData",0,1,8,"PackBoolTags",0,8,9,"NextTagItem",8
 DC.B "FilterTagChanges",8,9,0,"MapTags",8,9,0,"AllocateTagItems",0,"CloneTagItems",8
 DC.B "FreeTagItems",8,"RefreshTagItemClones",8,9,"TagInArray",0,8,"FilterTagItems"
 DC.B 8,9,0,"CallHookPkt",8,10,9,16,16,"Amiga2Date",0,8,"Date2Amiga",8,"CheckDate",8
 DC.B "Smult32",0,1,"Umult32",0,1,"SdivMod32",0,1,"UdivMod32",0,1,"Stricmp",8,9
 DC.B "Strnicmp",8,9,0,"ToUpper",0,"ToLower",0,"ApplyTagChanges",8,9,"Smult64",0,1
 DC.B "Umult64",0,1,"PackStructureTags",8,9,10,"UnpackStructureTags",8,9,10
 DC.B "AddNamedObject",8,9,"AllocNamedObjectA",8,9,"AttemptRemNamedObject",8
 DC.B "FindNamedObject",8,9,10,"FreeNamedObject",8,"NamedObjectName",8,"ReleaseNamedObject"
 DC.B 8,"RemNamedObject",8,9,"GetUniqueID",16,"ÿ",0,0,0,0

 EVEN
;*-*
;; #date data
LongMonths:
    DC.L    .01,.02,.03,.04,.05,.06,.07,.08,.09,.10,.11,.12
.01:DC.B    "January",0
.02:DC.B    "February",0
.03:DC.B    "March",0
.04:DC.B    "April",0
.05:DC.B    "May",0
.06:DC.B    "June",0
.07:DC.B    "July",0
.08:DC.B    "August",0
.09:DC.B    "September",0
.10:DC.B    "October",0
.11:DC.B    "November",0
.12:DC.B    "December",0
    EVEN
ShortMonths:
    DC.L    .01,.02,.03,.04,.05,.06,.07,.08,.09,.10,.11,.12
.01:DC.B    "Jan",0
.02:DC.B    "Feb",0
.03:DC.B    "Mar",0
.04:DC.B    "Apr",0
.05:DC.B    "May",0
.06:DC.B    "Jun",0
.07:DC.B    "Jul",0
.08:DC.B    "Aug",0
.09:DC.B    "Sep",0
.10:DC.B    "Oct",0
.11:DC.B    "Nov",0
.12:DC.B    "Dec",0
    EVEN

LongDays:
    DC.L    .0,.1,.2,.3,.4,.5,.6
.0: DC.B    "Sunday",0
.1: DC.B    "Monday",0
.2: DC.B    "Tuesday",0
.3: DC.B    "Wednesday",0
.4: DC.B    "Thursday",0
.5: DC.B    "Friday",0
.6: DC.B    "Saturday",0
    EVEN

ShortDays:
    DC.L    .0,.1,.2,.3,.4,.5,.6
.0: DC.B    "Su",0
.1: DC.B    "Mo",0
.2: DC.B    "Tu",0
.3: DC.B    "We",0
.4: DC.B    "Th",0
.5: DC.B    "Fr",0
.6: DC.B    "Sa",0
    EVEN
;*-*
;; Errors
ERRORTAB:
    DC.L     .0, .1, .2, .3, .4, .5, .6, .7, .8, .9
    DC.L    .10,.11,.12,.13,.14,.15,.16,.17,.18,.19
    DC.L    .20,.21,.22,.23,.24,.25,.26,.27,.28,.29
    DC.L    .30,.31,.32,.33,.34,.35,.36,.37,.38,.39
    DC.L    .40,.41,.42,.43,.44,.45,.46,.47,.48,.49
    DC.L    .50,.51,.52,.53,.54,.55,.56,.57,.58,.59
    DC.L    .60,.61,.62,.63,.64,.65,.66,.67,.68,.69
    DC.L    .70,.71,.72,.73,.74,.75,.76,.77,.78,.79
    DC.L    .80,.81,.82,.83,.84,.85,.86,.87,.88,.89
    DC.L    .90,.91,.92,.93,.94,.95,.96,.97,.98,.99

    DC.L    .100,.101,.102,.103,.104,.105,.106,.107
    DC.L    .108

    DC.L    NOERMESSAGE

.0: DC.B    'syntax error',0
.1: DC.B    'unknown keyword/const',0
.2: DC.B    '":=" expected',0
.3: DC.B    'unexpected characters in line',0
.4: DC.B    'label expected',0
.5: DC.B    '"," expected',0
.6: DC.B    'variable expected',0
.7: DC.B    'value does not fit in 32 bit',0
.8: DC.B    'missing apostrophe/quote',0
.9: DC.B    'incoherent programstructure',0
.10:    DC.B    'illegal commandline option',0
.11:    DC.B    'division and multiplication 16bit only',0
.12:    DC.B    'superfluous items in expression/statement',0
.13:    DC.B    'procedure "main" not available',0
.14:    DC.B    'double declaration of label',0
.15:    DC.B    'unsafe use of "*" or "/"',0
.16:    DC.B    "reading sourcefile didn`t succeed",0
.17:    DC.B    "writing executable/module didn`t succeed",0
.18:    DC.B    'bad args (try: EC ?)',0
.19:    DC.B    'unknown/illegal addressing mode',0
.20:    DC.B    'unmatched parentheses',0
.21:    DC.B    'double declaration',0
.22:    DC.B    'unknown identifier',0
.23:    DC.B    'incorrect # of args or use of ()',0
.24:    DC.B    'unknown e/library function',0
.25:    DC.B    'illegal function call',0
.26:    DC.B    'unknown format code following "\"',0
.27:    DC.B    '/* not properly nested comment structure */',0
.28:    DC.B    'could not load binary',0
.29:    DC.B    '"}" expected',0
.30:    DC.B    'immediate value expected',0
.31:    DC.B    'incorrect size of value',0
.32:    DC.B    'no E code allowed in assembler mode',0
.33:    DC.B    'illegal/inappropriate type',0
.34:    DC.B    '"]" expected',0
.35:    DC.B    'statement out of local/global scope',0
.36:    DC.B    'could not read module correctly',0
.37:    DC.B    'workspace full!',0
.38:    DC.B    'not enough memory while (re-)allocating',0
.39:    DC.B    'incorrect object definition',0
.40:    DC.B    'illegal use of/reference to object',0
.41:    DC.B    'incomplete if-then-else expression',0
.42:    DC.B    'unknown object identifier',0
.43:    DC.B    'double declaration of object identifier',0
.44:    DC.B    'reference(s) out of 32k range: switch to LARGE model',0
.45:    DC.B    'reference(s) out of 256 byte range',0
.46:    DC.B    'too sizy expression',0
.47:    DC.B    'incomplete exception handler definition',0
.48:    DC.B    'not allowed in a module',0
.49:    DC.B    'allowed in modules only',0
.50:    DC.B    "this doesn't make sense",0
.51:    DC.B    'you need a newer version of EC for this :-)',0
.52:    DC.B    'no matching "["',0
.53:    DC.B    'this instruction needs a better CPU/FPU (see OPT)',0
.54:    DC.B    "object doesn't understand this method",0
.55:    DC.B    "method doesn't have same #of args as method of baseclass",0
.56:    DC.B    'too many register variables in this function',0
.57:    DC.B    "Linker can't find all symbols",0
.58:    DC.B    'could not open "mathieeesingbas.library"',0
.59:    DC.B    'illegal destructor definition',0
.60:    DC.B    'implicit initialisation of private members',0
.61:    DC.B    'double method declaration',0
.62:    DC.B    'object referenced by other object not found',0
.63:    DC.B    'unknown preprocessor keyword',0
.64:    DC.B    'illegal macro definition',0
.65:    DC.B    'incoherent #ifdef/#ifndef nesting',0
.66:    DC.B    'macro redefinition',0
.67:    DC.B    'syntax error in #ifdef/#ifndef/#endif',0
.68:    DC.B    'macro(s) nested too deep',0
.69:    DC.B    'method definition out of object/module scope',0
.70:    DC.B    'library definition problem',0
.71:    DC.B    'object not known at this point',0
.72:    DC.B    'unknown HEX value after \x',0
.73:    DC.B    'value expected',0
.74:    DC.B    '")" expected',0
.75:    DC.B    'even number expected',0
.76:    DC.B    'you need a newer OS for this',0
.77:    DC.B    'unable to open resource',0
.78:    DC.B    'this instruction needs a newer OS version (see OPT)',0
.79:    DC.B    'illegal size',0
.80:    DC.B    'fpu register expected',0
.81:    DC.B    '":" expected',0
.82:    DC.B    'mmu register expected',0
.83:    DC.B    'control register expected',0
.84:    DC.B    'cpu register expected',0
.85:    DC.B    'this instruction works only in pool mode',0
.86:    DC.B    'not allowed in library mode',0
.87:    DC.B    'sorry, feature is not available yet',0
.88:    DC.B    'syntax error in lib file',0
.89:    DC.B    'a4 storage not defined in startup code',0
.90:    DC.B    'only RTR and RTE allowed',0
.91:    DC.B    'illegal scale factor',0
.92:    DC.B    'address/pc register expected',0
.93:    DC.B    'value does not fit in 16 bit',0
.94:    DC.B    'value does not fit in 8 bit',0
.95:    DC.B    'address/data register expected',0
.96:    DC.B    'reg args not allowed in methods',0
.97:    dc.b    'illegal section definition',0
.98:    DC.B    '680x0 code not allowed in powerpc section',0
.99:    DC.B    'powerpc code not allowed in 680x0 section',0
.100:   DC.B    'general purpose register expected',0
.101:   DC.B    'unknown/illegal mnemonic',0
.102:   DC.B    'condition register expected',0
.103:   DC.B    'powerpc floating point register expected',0
.104:   DC.B    'fp condition register expected',0
.105:   DC.B    'special purpose register expected',0
.106:   DC.B    'time base register expected',0
.107:   DC.B    'better cpu/fpu/mmu/osversion required',0
.108:   DC.B    'stack too small; need at least 20000 bytes',0
    EVEN

NOERMESSAGE:
    DC.B    'no errors'
NEMEND:
    EVEN
;*-*
;; Warnings
WARNINGMESSY:
    DC.B    "WARNING: ",0       ; 9

WARNINGTAB:
    DC.L    .1,.2,.3,.4,.5,.6,.7,.8,.9

.1: DC.B    "A4/A5 used in inline assembly",0
.2: DC.B    "keep an eye on your stacksize",0
.3: DC.B    "stack is definitely too small",0
.4: DC.B    'suspicious use of "=" in void expression(s). (line %d)',0
.5: DC.B    "module changed OPT settings",0
.6: DC.B    "variable used as function",0
.7: DC.B    "code outside PROCs",0
.8: DC.B    "040/060 emulated instruction(s) used",0
.9: DC.B    " ",0
    EVEN
;*-*

;; AssemblerInstructions
ASM_INSTAB:
    DC.L    .1,0,.2,0,.3,0,.4,0,.5,0,.6,0,.7,0,.8,0,.9,0,.10,0,.11,0,.12,0,.13,0,.14,0,.15,0,.16,0,.17,0
    DC.L    .18,0,.19,0,.20,0,.21,0,.22,0,.23,0,.24,0,.25,0,.26,0,.27,0,.28,0,.29,0,.30,0,.31,0,.32,0
    DC.L    .33,0,.34,0,.35,0,.36,0,.37,0,.38,0,.39,0,.40,0,.41,0,.42,0,.43,0,.44,0,.45,0,.46,0
    DC.L    .47,0,.48,0,.49,0,.50,0,.51,0,.52,0,.53,0,.54,0,.55,0,.56,0,.57,0,.58,0,.59,0,.60,0
    DC.L    .61,0,.62,0,.63,0,.64,0,.65,0,.66,0,.67,0,.68,0,.69,0,.70,0,.71,0,.72,0,.73,0,.74,0
    DC.L    .75,0,.76,0,.77,0,.78,0,.79,0,.80,0,.81,0,.82,0,.83,0,.84,0,.85,0,.86,0,.87,0,.88,0
    DC.L    .89,0,.90,0,.91,0,.92,0,.93,0,.94,0,.95,0,.96,0,.97,0,.98,0,.99,0,.100,0,.101,0,.102,0
    DC.L    .103,0,.104,0,.105,0,.106,0,.107,0,.108,0,.109,0,.110,0,.111,0,.112,0
    DC.L    .113,0,.114,0,.115,0,.116,0,.117,0,.118,0,.119,0,.120,0,.121,0,.122,0
    DC.L    .123,0,.124,0,.125,0,.126,0,.127,0,.128,0,.129,0,.130,0,.131,0,.132,0
    DC.L    .133,0,.134,0,.135,0,.136,0,.137,0,.138,0,.139,0,.140,0,.141,0,.142,0
    DC.L    .143,0,.144,0,.145,0,.146,0,.147,0,.148,0,.149,0,.150,0,.151,0,.152,0
    DC.L    .153,0,.154,0,.155,0,.156,0,.157,0,.158,0,.159,0,.160,0,.161,0,.162,0
    DC.L    .163,0,.164,0,.165,0,.166,0,.167,0,.168,0,.169,0,.170,0,.171,0,.172,0
    DC.L    .173,0,.174,0,.175,0,.176,0,.177,0,.178,0,.179,0,.180,0,.181,0,.182,0
    DC.L    .183,0,.184,0,.185,0,.186,0,.187,0,.188,0,.189,0,.190,0,.191,0,.192,0
    DC.L    .193,0,.194,0,.195,0,.196,0,.197,0,.198,0,.199,0,.200,0,.201,0,.202,0
    DC.L    .203,0,.204,0,.205,0,.206,0,.207,0,.208,0,.209,0,.210,0,.211,0,.212,0
    DC.L    .213,0,.214,0,.215,0,.216,0,.217,0,.218,0,.219,0,.220,0,.221,0,.222,0
    DC.L    .223,0,.224,0,.225,0,.226,0,.227,0,.228,0,.229,0,.230,0,.231,0,.232,0
    DC.L    .233,0,.234,0,.235,0,.236,0,.237,0,.238,0,.239,0,.240,0,.241,0,.242,0
    DC.L    .243,0,.244,0,.245,0,.246,0,.247,0,.248,0,.249,0,.250,0,.251,0,.252,0
    DC.L    .253,0,.254,0,.255,0,.256,0,.257,0,.258,0,.259,0,.260,0,.261,0,.262,0
    DC.L    .263,0,.264,0,.265,0,.266,0,.267,0,.268,0,.269,0,.270,0,.271,0,.272,0
    DC.L    .273,0,.274,0,.275,0,.276,0,.277,0,.278,0,.279,0,.280,0,.281,0,.282,0
    DC.L    .283,0,.284,0,.285,0,.286,0,.287,0,.288,0,.289,0,.290,0,.291,0,.292,0
    DC.L    .293,0,.294,0,.295,0,.296,0,.297,0,.298,0,.299,0,.300,0,.301,0,.302,0
    DC.L    .303,0,.304,0,.305,0,.306,0,.307,0,.308,0,.309,0,.310,0,.311,0,.312,0
    DC.L    .313,0,.314,0,.315,0,.316,0,.317,0,.318,0,.319,0,.320,0,.321,0,.322,0
    DC.L    .323,0,.324,0,.325,0,.326,0,.327,0,.328,0,.329,0,.330,0,.331,0,.332,0
    DC.L    .333,0,.334,0,.335,0,.336,0,.337,0,.338,0,.339,0,.340,0,.341,0,.342,0
    DC.L    .343,0,.344,0,.345,0,.346,0,.347,0,.348,0,.349,0,.350,0,.351,0,.352,0
    DC.L    .353,0,.354,0,.355,0,.356,0,.357,0,.358,0,.359,0,.360,0,.361,0,.362,0
    DC.L    .363,0,.364,0,.365,0,.366,0,.367,0,.368,0,.369,0,.370,0,.371,0,.372,0
    DC.L    .373,0,.374,0,.375,0,.376,0,.377,0,.378,0,.379,0,.380,0,.381,0,.382,0
    DC.L    .383,0,.384,0,.385,0,.386,0,.387,0,.388,0,.389,0,.390,0,.391,0,.392,0
    DC.L    .393,0,.394,0,.395,0,.396,0,.397,0,.398,0,.399,0,.400,0,.401,0,.402,0
    DC.L    .403,0,.404,0,.405,0,.406,0,.407,0,.408,0,.409,0,.410,0,.411,0,.412,0
    DC.L    .413,0,.414,0,.415,0,.416,0,.417,0,.418,0,.419,0,.420,0,.421,0,.422,0

    DC.L    0,0

.1: DC.B    "MOVE",0
.2: DC.B    "RTS",0
.3: DC.B    "ADD",0
.4: DC.B    "SUB",0
.5: DC.B    "MOVEQ",0
.6: DC.B    "ASR",0
.7: DC.B    "BSR",0
.8: DC.B    "CLR",0
.9: DC.B    "CMP",0
.10:    DC.B    "EOR",0
.11:    DC.B    "JMP",0
.12:    DC.B    "JSR",0
.13:    DC.B    "LSL",0
.14:    DC.B    "LSR",0
.15:    DC.B    "SUBQ",0
.16:    DC.B    "ADDQ",0
.17:    DC.B    "OR",0
.18:    DC.B    "ROL",0
.19:    DC.B    "ROR",0
.20:    DC.B    "AND",0
.21:    DC.B    "ASL",0
.22:    DC.B    "BTST",0
.23:    DC.B    "MOVEM",0
.24:    DC.B    "ADDA",0
.25:    DC.B    "ADDI",0
.26:    DC.B    "ADDX",0
.27:    DC.B    "CMPA",0
.28:    DC.B    "CMPM",0
.29:    DC.B    "CMPI",0
.30:    DC.B    "MOVEA",0
.31:    DC.B    "BRA",0
.32:    DC.B    "NEGX",0
.33:    DC.B    "ORI",0
.34:    DC.B    "SUBA",0
.35:    DC.B    "SUBI",0
.36:    DC.B    "SUBX",0
.37:    DC.B    "NOT",0
.38:    DC.B    "MULS",0
.39:    DC.B    "MULU",0
.40:    DC.B    "NEG",0
.41:    DC.B    "TST",0
.42:    DC.B    "SWAP",0
.43:    DC.B    "LEA",0
.44:    DC.B    "DIVS",0
.45:    DC.B    "DIVU",0
.46:    DC.B    "EXG",0
.47:    DC.B    "STOP",0
.48:    DC.B    "RTR",0
.49:    DC.B    "RTE",0
.50:    DC.B    "ABCD",0
.51:    DC.B    "SBCD",0
.52:    DC.B    "MOVEP",0
.53:    DC.B    "NOP",0
.54:    DC.B    "ROXL",0
.55:    DC.B    "ROXR",0
.56:    DC.B    "BCHG",0
.57:    DC.B    "BCLR",0
.58:    DC.B    "BSET",0
.59:    DC.B    "CHK",0
.60:    DC.B    "EXT",0
.61:    DC.B    "LINK",0
.62:    DC.B    "UNLK",0
.63:    DC.B    "NBCD",0
.64:    DC.B    "PEA",0
.65:    DC.B    "RESET",0
.66:    DC.B    "TRAP",0
.67:    DC.B    "TRAPV",0
.68:    DC.B    "BHI",0
.69:    DC.B    "BLS",0
.70:    DC.B    "BCC",0
.71:    DC.B    "BCS",0
.72:    DC.B    "BNE",0
.73:    DC.B    "BEQ",0
.74:    DC.B    "BVC",0
.75:    DC.B    "BVS",0
.76:    DC.B    "BPL",0
.77:    DC.B    "BMI",0
.78:    DC.B    "BGE",0
.79:    DC.B    "BLT",0
.80:    DC.B    "BGT",0
.81:    DC.B    "BLE",0
.82:    DC.B    "DBRA",0
.83:    DC.B    "DBF",0
.84:    DC.B    "DBT",0
.85:    DC.B    "DBHI",0
.86:    DC.B    "DBLS",0
.87:    DC.B    "DBCC",0
.88:    DC.B    "DBCS",0
.89:    DC.B    "DBNE",0
.90:    DC.B    "DBEQ",0
.91:    DC.B    "DBVC",0
.92:    DC.B    "DBVS",0
.93:    DC.B    "DBPL",0
.94:    DC.B    "DBMI",0
.95:    DC.B    "DBGE",0
.96:    DC.B    "DBLT",0
.97:    DC.B    "DBGT",0
.98:    DC.B    "DBLE",0
.99:    DC.B    "SHI",0
.100:   DC.B    "SLS",0
.101:   DC.B    "SCC",0
.102:   DC.B    "SCS",0
.103:   DC.B    "SNE",0
.104:   DC.B    "SEQ",0
.105:   DC.B    "SVC",0
.106:   DC.B    "SVS",0
.107:   DC.B    "SPL",0
.108:   DC.B    "SMI",0
.109:   DC.B    "SGE",0
.110:   DC.B    "SLT",0
.111:   DC.B    "SGT",0
.112:   DC.B    "SLE",0
.113:   DC.B    "ST",0
.114:   DC.B    "SF",0
.115:   DC.B    "ANDI",0
.116:   DC.B    "EORI",0
.117:   DC.B    "ILLEGAL",0
.118:   DC.B    "RTD",0
.119:   DC.B    "FABS",0
.120:   DC.B    "FACOS",0
.121:   DC.B    "FASIN",0
.122:   DC.B    "FATAN",0
.123:   DC.B    "FATANH",0
.124:   DC.B    "FCOS",0
.125:   DC.B    "FCOSH",0
.126:   DC.B    "FETOX",0
.127:   DC.B    "FETOXM1",0
.128:   DC.B    "FGETEXP",0
.129:   DC.B    "FGETMAN",0
.130:   DC.B    "FINT",0
.131:   DC.B    "FINRZ",0
.132:   DC.B    "FLOG10",0
.133:   DC.B    "FLOG2",0
.134:   DC.B    "FLOGN",0
.135:   DC.B    "FLOGNP1",0
.136:   DC.B    "FNEG",0
.137:   DC.B    "FSIN",0
.138:   DC.B    "FSINH",0
.139:   DC.B    "FSQRT",0
.140:   DC.B    "FTAN",0
.141:   DC.B    "FTANH",0
.142:   DC.B    "FTENTOX",0
.143:   DC.B    "FTWOTOX",0
.144:   DC.B    "FADD",0
.145:   DC.B    "FCMP",0
.146:   DC.B    "FDIV",0
.147:   DC.B    "FMOD",0
.148:   DC.B    "FMUL",0
.149:   DC.B    "FREM",0
.150:   DC.B    "FSCALE",0
.151:   DC.B    "FSGLDIV",0
.152:   DC.B    "FSGLMUL",0
.153:   DC.B    "FSUB",0
.154:   DC.B    "FBF",0
.155:   DC.B    "FBEQ",0
.156:   DC.B    "FBOGT",0
.157:   DC.B    "FBOGE",0
.158:   DC.B    "FBOLT",0
.159:   DC.B    "FBOLE",0
.160:   DC.B    "FBOGL",0
.161:   DC.B    "FBOR",0
.162:   DC.B    "FBUN",0
.163:   DC.B    "FBUNE",0
.164:   DC.B    "FBUGT",0
.165:   DC.B    "FBUGE",0
.166:   DC.B    "FBULT",0
.167:   DC.B    "FBULE",0
.168:   DC.B    "FBNE",0
.169:   DC.B    "FBT",0
.170:   DC.B    "FBSF",0
.171:   DC.B    "FBSEQ",0
.172:   DC.B    "FBGT",0
.173:   DC.B    "FBGE",0
.174:   DC.B    "FBLT",0
.175:   DC.B    "FBLE",0
.176:   DC.B    "FBGL",0
.177:   DC.B    "FBGLE",0
.178:   DC.B    "FBNGLE",0
.179:   DC.B    "FBNGL",0
.180:   DC.B    "FBNLE",0
.181:   DC.B    "FBNLT",0
.182:   DC.B    "FBNGE",0
.183:   DC.B    "FBNGT",0
.184:   DC.B    "FBSNE",0
.185:   DC.B    "FBST",0
.186:   DC.B    "PBBS",0
.187:   DC.B    "PBBC",0
.188:   DC.B    "PBLS",0
.189:   DC.B    "PBLC",0
.190:   DC.B    "PBSS",0
.191:   DC.B    "PBSC",0
.192:   DC.B    "PBAS",0
.193:   DC.B    "PBAC",0
.194:   DC.B    "PBWS",0
.195:   DC.B    "PBWC",0
.196:   DC.B    "PBIS",0
.197:   DC.B    "PBIC",0
.198:   DC.B    "PBGS",0
.199:   DC.B    "PBGC",0
.200:   DC.B    "PBCS",0
.201:   DC.B    "PBCC",0
.202:   DC.B    "FSADD",0
.203:   DC.B    "FDADD",0
.204:   DC.B    "FSDIV",0
.205:   DC.B    "FDDIV",0
.206:   DC.B    "FSMOVE",0
.207:   DC.B    "FDMOVE",0
.208:   DC.B    "FSMUL",0
.209:   DC.B    "FDMUL",0
.210:   DC.B    "FSNEG",0
.211:   DC.B    "FDNEG",0
.212:   DC.B    "FSSQRT",0
.213:   DC.B    "FDSQRT",0
.214:   DC.B    "FSSUB",0
.215:   DC.B    "FDSUB",0
.216:   DC.B    "FDBF",0
.217:   DC.B    "FDBEQ",0
.218:   DC.B    "FDBOGT",0
.219:   DC.B    "FDBOGE",0
.220:   DC.B    "FDBOLT",0
.221:   DC.B    "FDBOLE",0
.222:   DC.B    "FDBOGL",0
.223:   DC.B    "FDBOR",0
.224:   DC.B    "FDBUN",0
.225:   DC.B    "FDBUNE",0
.226:   DC.B    "FDBUGT",0
.227:   DC.B    "FDBUGE",0
.228:   DC.B    "FDBULT",0
.229:   DC.B    "FDBULE",0
.230:   DC.B    "FDBNE",0
.231:   DC.B    "FDBT",0
.232:   DC.B    "FDBSF",0
.233:   DC.B    "FDBSEQ",0
.234:   DC.B    "FDBGT",0
.235:   DC.B    "FDBGE",0
.236:   DC.B    "FDBLT",0
.237:   DC.B    "FDBLE",0
.238:   DC.B    "FDBGL",0
.239:   DC.B    "FDBGLE",0
.240:   DC.B    "FDBNGLE",0
.241:   DC.B    "FDBNGL",0
.242:   DC.B    "FDBNLE",0
.243:   DC.B    "FDBNLT",0
.244:   DC.B    "FDBNGE",0
.245:   DC.B    "FDBNGT",0
.246:   DC.B    "FDBSNE",0
.247:   DC.B    "FDBST",0
.248:   DC.B    "PDBBS",0
.249:   DC.B    "PDBBC",0
.250:   DC.B    "PDBLS",0
.251:   DC.B    "PDBLC",0
.252:   DC.B    "PDBSS",0
.253:   DC.B    "PDBSC",0
.254:   DC.B    "PDBAS",0
.255:   DC.B    "PDBAC",0
.256:   DC.B    "PDBWS",0
.257:   DC.B    "PDBWC",0
.258:   DC.B    "PDBIS",0
.259:   DC.B    "PDBIC",0
.260:   DC.B    "PDBGS",0
.261:   DC.B    "PDBGC",0
.262:   DC.B    "PDBCS",0
.263:   DC.B    "PDBCC",0
.264:   DC.B    "FSF",0
.265:   DC.B    "FSEQ",0
.266:   DC.B    "FSOGT",0
.267:   DC.B    "FSOGE",0
.268:   DC.B    "FSOLT",0
.269:   DC.B    "FSOLE",0
.270:   DC.B    "FSOGL",0
.271:   DC.B    "FSOR",0
.272:   DC.B    "FSUN",0
.273:   DC.B    "FSUNE",0
.274:   DC.B    "FSUGT",0
.275:   DC.B    "FSUGE",0
.276:   DC.B    "FSULT",0
.277:   DC.B    "FSULE",0
.278:   DC.B    "FSNE",0
.279:   DC.B    "FST",0
.280:   DC.B    "FSSF",0
.281:   DC.B    "FSSEQ",0
.282:   DC.B    "FSGT",0
.283:   DC.B    "FSGE",0
.284:   DC.B    "FSLT",0
.285:   DC.B    "FSLE",0
.286:   DC.B    "FSGL",0
.287:   DC.B    "FSGLE",0
.288:   DC.B    "FSNGLE",0
.289:   DC.B    "FSNGL",0
.290:   DC.B    "FSNLE",0
.291:   DC.B    "FSNLT",0
.292:   DC.B    "FSNGE",0
.293:   DC.B    "FSNGT",0
.294:   DC.B    "FSSNE",0
.295:   DC.B    "FSST",0
.296:   DC.B    "PSBS",0
.297:   DC.B    "PSBC",0
.298:   DC.B    "PSLS",0
.299:   DC.B    "PSLC",0
.300:   DC.B    "PDSS",0
.301:   DC.B    "PSSC",0
.302:   DC.B    "PSAS",0
.303:   DC.B    "PSAC",0
.304:   DC.B    "PSWS",0
.305:   DC.B    "PSWC",0
.306:   DC.B    "PSIS",0
.307:   DC.B    "PSIC",0
.308:   DC.B    "PSGS",0
.309:   DC.B    "PSGC",0
.310:   DC.B    "PSCS",0
.311:   DC.B    "PSCC",0
.312:   DC.B    "FMOVE",0
.313:   DC.B    "FNOP",0
.314:   DC.B    "FMOVECR",0
.315:   DC.B    "FMOVEM",0
.316:   DC.B    "FRESTORE",0
.317:   DC.B    "FSAVE",0
.318:   DC.B    "FSINCOS",0
.319:   DC.B    "FTRAPF",0
.320:   DC.B    "FTRAPEQ",0
.321:   DC.B    "FTRAPOGT",0
.322:   DC.B    "FTRAPOGE",0
.323:   DC.B    "FTRAPOLT",0
.324:   DC.B    "FTRAPOLE",0
.325:   DC.B    "FTRAPOGL",0
.326:   DC.B    "FTRAPOR",0
.327:   DC.B    "FTRAPUN",0
.328:   DC.B    "FTRAPUNE",0
.329:   DC.B    "FTRAPUGT",0
.330:   DC.B    "FTRAPUGE",0
.331:   DC.B    "FTRAPULT",0
.332:   DC.B    "FTRAPULE",0
.333:   DC.B    "FTRAPNE",0
.334:   DC.B    "FTRAPT",0
.335:   DC.B    "FTRAPSF",0
.336:   DC.B    "FTRAPSEQ",0
.337:   DC.B    "FTRAPGT",0
.338:   DC.B    "FTRAPGE",0
.339:   DC.B    "FTRAPLT",0
.340:   DC.B    "FTRAPLE",0
.341:   DC.B    "FTRAPGL",0
.342:   DC.B    "FTRAPGLE",0
.343:   DC.B    "FTRAPNGLE",0
.344:   DC.B    "FTRAPNGL",0
.345:   DC.B    "FTRAPNLE",0
.346:   DC.B    "FTRAPNLT",0
.347:   DC.B    "FTRAPNGE",0
.348:   DC.B    "FTRAPNGT",0
.349:   DC.B    "FTRAPSNE",0
.350:   DC.B    "FTRAPST",0
.351:   DC.B    "PTRAPBS",0
.352:   DC.B    "PTRAPBC",0
.353:   DC.B    "PTRAPLS",0
.354:   DC.B    "PTRAPLC",0
.355:   DC.B    "PTRAPSS",0
.356:   DC.B    "PTRAPSC",0
.357:   DC.B    "PTRAPAS",0
.358:   DC.B    "PTRAPAC",0
.359:   DC.B    "PTRAPWS",0
.360:   DC.B    "PTRAPWC",0
.361:   DC.B    "PTRAPIS",0
.362:   DC.B    "PTRAPIC",0
.363:   DC.B    "PTRAPGS",0
.364:   DC.B    "PTRAPGC",0
.365:   DC.B    "PTRAPCS",0
.366:   DC.B    "PTRAPCC",0
.367:   DC.B    "FTST",0
.368:   DC.B    "PSAVE",0
.369:   DC.B    "PRESTORE",0
.370:   DC.B    "PFLUSHA",0
.371:   DC.B    "PFLUSH",0
.372:   DC.B    "PFLUSHS",0
.373:   DC.B    "PFLUSHR",0
.374:   DC.B    "PLOADR",0
.375:   DC.B    "PLOADW",0
.376:   DC.B    "PTESTR",0
.377:   DC.B    "PTESTW",0
.378:   DC.B    "PMOVE",0
.379:   DC.B    "PFLUSHAN",0
.380:   DC.B    "PFLUSHN",0
.381:   DC.B    "PLPAR",0
.382:   DC.B    "PLPAW",0
.383:   DC.B    "BKPT",0
.384:   DC.B    "MOVEC",0
.385:   DC.B    "MOVES",0
.386:   DC.B    "BFCHG",0
.387:   DC.B    "BFCLR",0
.388:   DC.B    "BFEXTS",0
.389:   DC.B    "BFEXTU",0
.390:   DC.B    "BFFFO",0
.391:   DC.B    "BFSET",0
.392:   DC.B    "BFTST",0
.393:   DC.B    "BFINS",0
.394:   DC.B    "DIVSL",0
.395:   DC.B    "DIVUL",0
.396:   DC.B    "EXTB",0
.397:   DC.B    "TRAPCC",0
.398:   DC.B    "TRAPCS",0
.399:   DC.B    "TRAPEQ",0
.400:   DC.B    "TRAPF",0
.401:   DC.B    "TRAPGE",0
.402:   DC.B    "TRAPGT",0
.403:   DC.B    "TRAPHI",0
.404:   DC.B    "TRAPLE",0
.405:   DC.B    "TRAPLS",0
.406:   DC.B    "TRAPLT",0
.407:   DC.B    "TRAPMI",0
.408:   DC.B    "TRAPNE",0
.409:   DC.B    "TRAPPL",0
.410:   DC.B    "TRAPT",0
.411:   DC.B    "TRAPVC",0
.412:   DC.B    "TRAPVS",0
.413:   DC.B    "CINVL",0
.414:   DC.B    "CINVP",0
.415:   DC.B    "CINVA",0
.416:   DC.B    "CPUSHL",0
.417:   DC.B    "CPUSHP",0
.418:   DC.B    "CPUSHA",0
.419:   DC.B    "MOVE16",0
.420:   DC.B    "LPSTOP",0
.421:   DC.B    "HALT",0
.422:   DC.B    "PULSE",0
    EVEN
;*-*
;; AssemblerJobs
; ADDR. MODE MASKS

; TODO:
;  - MOVE -> Condition register support
;  - Implement: CALLM, CAS, CAS2, CHK2, CMP2, PACK, UNPK, RTM
;  -

;; Sizes
;----------------QDPXSLWB---

SIZE_BWL=       %00000111
SIZE_SBWL=      %00001111
SIZE_BL=        %00000101
SIZE_WL=        %00000110
SIZE_L=         %00000100
SIZE_B=         %00000001
SIZE_BWLSXPD=   %01111111
SIZE_S=         %00001000
SIZE_D=         %01000000
SIZE_W=         %00000010
SIZE_BWLSXD=    %01011111
SIZE_X=         %00010000
SIZE_LX=        %00010100
SIZE_WLQ=       %10000110
;*-*
;; Adressing modes
ASRC    = %111111111111
NASRC   = %111111111101
MMSRC   = %011111101100
PSRC    = %011111100000

DEST    = %000111111101
MDEST   = %000111111100
QDEST   = %000111111111
BTDEST  = %011111111101
MMDEST  = %011111110100
PMDEST  = %011111111100

JLEA    = %011111100100
FSAVEM  = %000111110100
FRESTM  = %000111101100
;*-*
;; Jobs
;---------------------------------------

SIZE    = 2
SIZEM   = 3
SIZEA   = 16
SIZEE   = 17
SIZEF   = 26    ; FIXED SIZE

EA  = 4
EAM = 5

COMMA   = 6

ADDSUB  = 7
IMMMQ   = 8
IMMQ    = 9

BCC = 10
DBCC    = 11
DOEXG   = 19
CMPM    = 20
MOVEM   = 21
TRAP    = 22
MOVEP   = 23
CMP_I   = 24

ADRREG  = 12
DATAREG = 14

SHIFT   = 13
BIT = 15
XREG    = 18

CPUSUP  = 25
SIZE0   = 27
SIZEO   = 28
FPM     = 29
FPUSUP  = 30
WARN46  = 31
FPD     = 32
MMUSUP  = 33
COPY    = 34
FPMOVE  = 35
FMOVECR = 36
FMOVEM  = 37
FSINCOS = 38
TRAPCC  = 39
FTST    = 40
PMMSTD  = 41
PMMLDX  = 42
PMMTST  = 43
PMMMOVE = 44
PMMEA   = 45
IMMBK   = 46
MOVEC   = 47
MOVES   = 48
BITFLD1 = 49
BITFLD2 = 50
BITFLD3 = 51
MULDIV  = 52
LINK    = 53
CPCACHE = 54
MOVE16  = 55
LPSTOP  = 56
DOMOVE  = 57
DOANDI  = 58
DOEORI  = 59
DOORI   = 60
;*-*
ASM_INSJOB:
    DC.L    .1,.2,.3,.4,.5,.6,.7,.8,.9,.10,.11,.12,.13,.14,.15,.16,.17
    DC.L    .18,.19,.20,.21,.22,.23,.24,.25,.26,.27,.28,.29,.30,.31,.32
    DC.L    .33,.34,.35,.36,.37,.38,.39,.40,.41,.42,.43,.44,.45,.46
    DC.L    .47,.48,.49,.50,.51,.52,.53,.54,.55,.56,.57,.58,.59,.60
    DC.L    .61,.62,.63,.64,.65,.66,.67,.68,.69,.70,.71,.72,.73,.74
    DC.L    .75,.76,.77,.78,.79,.80,.81,.82,.83,.84,.85,.86,.87,.88
    DC.L    .89,.90,.91,.92,.93,.94,.95,.96,.97,.98,.99,.100,.101,.102
    DC.L    .103,.104,.105,.106,.107,.108,.109,.110,.111,.112
    DC.L    .113,.114,.115,.116,.117,.118,.119,.120,.121,.122
    DC.L    .123,.124,.125,.126,.127,.128,.129,.130,.131,.132
    DC.L    .133,.134,.135,.136,.137,.138,.139,.140,.141,.142
    DC.L    .143,.144,.145,.146,.147,.148,.149,.150,.151,.152
    DC.L    .153,.154,.155,.156,.157,.158,.159,.160,.161,.162
    DC.L    .163,.164,.165,.166,.167,.168,.169,.170,.171,.172
    DC.L    .173,.174,.175,.176,.177,.178,.179,.180,.181,.182
    DC.L    .183,.184,.185,.186,.187,.188,.189,.190,.191,.192
    DC.L    .193,.194,.195,.196,.197,.198,.199,.200,.201,.202
    DC.L    .203,.204,.205,.206,.207,.208,.209,.210,.211,.212
    DC.L    .213,.214,.215,.216,.217,.218,.219,.220,.221,.222
    DC.L    .223,.224,.225,.226,.227,.228,.229,.230,.231,.232
    DC.L    .233,.234,.235,.236,.237,.238,.239,.240,.241,.242
    DC.L    .243,.244,.245,.246,.247,.248,.249,.250,.251,.252
    DC.L    .253,.254,.255,.256,.257,.258,.259,.260,.261,.262
    DC.L    .263,.264,.265,.266,.267,.268,.269,.270,.271,.272
    DC.L    .273,.274,.275,.276,.277,.278,.279,.280,.281,.282
    DC.L    .283,.284,.285,.286,.287,.288,.289,.290,.291,.292
    DC.L    .293,.294,.295,.296,.297,.298,.299,.300,.301,.302
    DC.L    .303,.304,.305,.306,.307,.308,.309,.310,.311,.312
    DC.L    .313,.314,.315,.316,.317,.318,.319,.320,.321,.322
    DC.L    .323,.324,.325,.326,.327,.328,.329,.330,.331,.332
    DC.L    .333,.334,.335,.336,.337,.338,.339,.340,.341,.342
    DC.L    .343,.344,.345,.346,.347,.348,.349,.350,.351,.352
    DC.L    .353,.354,.355,.356,.357,.358,.359,.360,.361,.362
    DC.L    .363,.364,.365,.366,.367,.368,.369,.370,.371,.372
    DC.L    .373,.374,.375,.376,.377,.378,.379,.380,.381,.382
    DC.L    .383,.384,.385,.386,.387,.388,.389,.390,.391,.392
    DC.L    .393,.394,.395,.396,.397,.398,.399,.400,.401,.402
    DC.L    .403,.404,.405,.406,.407,.408,.409,.410,.411,.412
    DC.L    .413,.414,.415,.416,.417,.418,.419,.420,.421,.422

    DC.L    0

.1:     DC.W    $0000,SIZEM,2,SIZE_BWL,12,DOMOVE,0                                      ; MOVE
.2:     DC.W    $4E75,SIZE0,0                                                           ; RTS
.3:     DC.W    $D000,SIZE,2,SIZE_BWL,6,ADDSUB,$0600,0                                  ; ADD
.4:     DC.W    $9000,SIZE,2,SIZE_BWL,6,ADDSUB,$0400,0                                  ; SUB
.5:     DC.W    $7000,SIZEO,3,SIZE_L,IMMMQ,0                                            ; MOVEQ
.6:     DC.W    $E000,SIZEO,2,SIZE_BWL,SHIFT,$E0C0,0                                    ; ASR
.7:     DC.W    $6100,SIZEO,2,SIZE_SBWL,BCC,$61FF,0                                     ; BSR
.8:     DC.W    $4200,SIZE,2,SIZE_BWL,6,EA,DEST,0,0                                     ; CLR
.9:     DC.W    $B000,SIZE,2,SIZE_BWL,6,CMP_I,EA,ASRC,0,COMMA,DATAREG,9,0               ; CMP
.10:    DC.W    $B100,SIZE,2,SIZE_BWL,6,DATAREG,9,COMMA,EA,DEST,0,0                     ; EOR
.11:    DC.W    $4EC0,SIZE0,EA,JLEA,0,0                                                 ; JMP
.12:    DC.W    $4E80,SIZE0,EA,JLEA,0,0                                                 ; JSR
.13:    DC.W    $E108,SIZEO,2,SIZE_BWL,SHIFT,$E3C0,0                                    ; LSL
.14:    DC.W    $E008,SIZEO,2,SIZE_BWL,SHIFT,$E2C0,0                                    ; LSR
.15:    DC.W    $5100,SIZE,2,SIZE_BWL,6,IMMQ,COMMA,EA,QDEST,0,0                         ; SUBQ
.16:    DC.W    $5000,SIZE,2,SIZE_BWL,6,IMMQ,COMMA,EA,QDEST,0,0                         ; ADDQ
.17:    DC.W    $8000,SIZE,2,SIZE_BWL,6,ADDSUB,0,0                                      ; OR
.18:    DC.W    $E118,SIZEO,2,SIZE_BWL,SHIFT,$E7C0,0                                    ; ROL
.19:    DC.W    $E018,SIZEO,2,SIZE_BWL,SHIFT,$E6C0,0                                    ; ROR
.20:    DC.W    $C000,SIZE,2,SIZE_BWL,6,ADDSUB,$0200,0                                  ; AND
.21:    DC.W    $E100,SIZEO,2,SIZE_BWL,SHIFT,$E1C0,0                                    ; ASL
.22:    DC.W    $0100,SIZEO,2,SIZE_BL,BIT,$0800,0                                       ; BTST
.23:    DC.W    $4880,SIZEE,2,SIZE_WL,MOVEM,0                                           ; MOVEM
.24:    DC.W    $D0C0,SIZEA,2,SIZE_WL,EA,ASRC,0,COMMA,ADRREG,9,0                        ; ADDA
.25:    DC.W    $0600,SIZE,2,SIZE_BWL,6,EA,$800,-1,COMMA,EA,DEST,0,0                    ; ADDI
.26:    DC.W    $D100,SIZE,2,SIZE_BWL,6,XREG,0                                          ; ADDX
.27:    DC.W    $B0C0,SIZEA,2,SIZE_WL,EA,ASRC,0,COMMA,ADRREG,9,0                        ; CMPA
.28:    DC.W    $B108,SIZE,2,SIZE_BWL,6,CMPM,0                                          ; CMPM
.29:    DC.W    $0C00,SIZE,2,SIZE_BWL,6,EA,$800,-1,COMMA,EA,DEST,0,0                    ; CMPI
.30:    DC.W    $0040,SIZEM,2,SIZE_WL,12,EA,ASRC,0,COMMA,ADRREG,9,0                     ; MOVEA
.31:    DC.W    $6000,SIZEO,2,SIZE_SBWL,BCC,$60FF,0                                     ; BRA
.32:    DC.W    $4000,SIZE,2,SIZE_BWL,6,EA,DEST,0,0                                     ; NEGX
.33:    DC.W    $0000,SIZE,2,SIZE_BWL,6,DOORI,0                                         ; ORI
.34:    DC.W    $90C0,SIZEA,2,SIZE_WL,EA,ASRC,0,COMMA,ADRREG,9,0                        ; SUBA
.35:    DC.W    $0400,SIZE,2,SIZE_BWL,6,EA,$800,-1,COMMA,EA,DEST,0,0                    ; SUBI
.36:    DC.W    $9100,SIZE,2,SIZE_BWL,6,XREG,0                                          ; SUBX
.37:    DC.W    $4600,SIZE,2,SIZE_BWL,6,EA,DEST,0,0                                     ; NOT
.38:    DC.W    $C1C0,SIZEO,2,SIZE_WL,MULDIV,$4c00,$0c00,0                              ; MULS
.39:    DC.W    $C0C0,SIZEO,2,SIZE_WL,MULDIV,$4c00,$0400,0                              ; MULU
.40:    DC.W    $4400,SIZE,2,SIZE_BWL,6,EA,DEST,0,0                                     ; NEG
.41:    DC.W    $4A00,SIZE,2,SIZE_BWL,6,EA,DEST,0,0                                     ; TST
.42:    DC.W    $4840,SIZE0,DATAREG,0,0                                                 ; SWAP
.43:    DC.W    $41C0,SIZEO,3,SIZE_L,EA,JLEA,0,COMMA,ADRREG,9,0                         ; LEA
.44:    DC.W    $81C0,SIZEO,2,SIZE_WL,MULDIV,$4c40,$0c00,0                              ; DIVS
.45:    DC.W    $80C0,SIZEO,2,SIZE_WL,MULDIV,$4c40,$0400,0                              ; DIVU
.46:    DC.W    $C100,SIZE0,DOEXG,0                                                     ; EXG
.47:    DC.W    $4E72,SIZE0,EA,$800,-1,0                                                ; STOP
.48:    DC.W    $4E77,SIZE0,0                                                           ; RTR
.49:    DC.W    $4E73,SIZE0,0                                                           ; RTE
.50:    DC.W    $C100,SIZEO,1,SIZE_B,XREG,0                                             ; ABCD
.51:    DC.W    $8100,SIZEO,1,SIZE_B,XREG,0                                             ; SBCD
.52:    DC.W    $0108,SIZEE,2,SIZE_WL,MOVEP,0                                           ; MOVEP
.53:    DC.W    $4E71,SIZE0,0                                                           ; NOP
.54:    DC.W    $E110,SIZEO,2,SIZE_BWL,SHIFT,$E5C0,0                                    ; ROXL
.55:    DC.W    $E010,SIZEO,2,SIZE_BWL,SHIFT,$E4C0,0                                    ; ROXR
.56:    DC.W    $0140,SIZEO,3,SIZE_BL,BIT,$0840,0                                       ; BCHG
.57:    DC.W    $0180,SIZEO,3,SIZE_BL,BIT,$0880,0                                       ; BCLR
.58:    DC.W    $01C0,SIZEO,3,SIZE_BL,BIT,$08C0,0                                       ; BSET
.59:    DC.W    $4180,SIZEO,2,SIZE_WL,EA,NASRC,0,COMMA,DATAREG,9,0                      ; CHK
.60:    DC.W    $4880,SIZEE,2,SIZE_WL,DATAREG,0,0                                       ; EXT
.61:    DC.W    $4E50,SIZEO,2,SIZE_WL,LINK,0                                            ; LINK
.62:    DC.W    $4E58,SIZE0,ADRREG,0,0                                                  ; UNLK
.63:    DC.W    $4800,SIZEO,1,SIZE_B,EA,DEST,0,0                                        ; NBCD
.64:    DC.W    $4840,SIZEO,3,SIZE_L,EA,PSRC,0,0                                        ; PEA
.65:    DC.W    $4E70,SIZE0,0                                                           ; RESET
.66:    DC.W    $4E40,SIZE0,TRAP,0                                                      ; TRAP
.67:    DC.W    $4E76,SIZE0,0                                                           ; TRAPV
.68:    DC.W    $6200,SIZEO,2,SIZE_SBWL,BCC,$62FF,0                                     ; Bcc
.69:    DC.W    $6300,SIZEO,2,SIZE_SBWL,BCC,$63FF,0
.70:    DC.W    $6400,SIZEO,2,SIZE_SBWL,BCC,$64FF,0
.71:    DC.W    $6500,SIZEO,2,SIZE_SBWL,BCC,$65FF,0
.72:    DC.W    $6600,SIZEO,2,SIZE_SBWL,BCC,$66FF,0
.73:    DC.W    $6700,SIZEO,2,SIZE_SBWL,BCC,$67FF,0
.74:    DC.W    $6800,SIZEO,2,SIZE_SBWL,BCC,$68FF,0
.75:    DC.W    $6900,SIZEO,2,SIZE_SBWL,BCC,$69FF,0
.76:    DC.W    $6A00,SIZEO,2,SIZE_SBWL,BCC,$6AFF,0
.77:    DC.W    $6B00,SIZEO,2,SIZE_SBWL,BCC,$6BFF,0
.78:    DC.W    $6C00,SIZEO,2,SIZE_SBWL,BCC,$6CFF,0
.79:    DC.W    $6D00,SIZEO,2,SIZE_SBWL,BCC,$6DFF,0
.80:    DC.W    $6E00,SIZEO,2,SIZE_SBWL,BCC,$6EFF,0
.81:    DC.W    $6F00,SIZEO,2,SIZE_SBWL,BCC,$6FFF,0
.82:    DC.W    $51C8,SIZE0,DBCC,0                                                      ; DBRA
.83:    DC.W    $51C8,SIZE0,DBCC,0                                                      ; DBF
.84:    DC.W    $50C8,SIZE0,DBCC,0                                                      ; DBT
.85:    DC.W    $52C8,SIZE0,DBCC,0                                                      ; DBcc
.86:    DC.W    $53C8,SIZE0,DBCC,0
.87:    DC.W    $54C8,SIZE0,DBCC,0
.88:    DC.W    $55C8,SIZE0,DBCC,0
.89:    DC.W    $56C8,SIZE0,DBCC,0
.90:    DC.W    $57C8,SIZE0,DBCC,0
.91:    DC.W    $58C8,SIZE0,DBCC,0
.92:    DC.W    $59C8,SIZE0,DBCC,0
.93:    DC.W    $5AC8,SIZE0,DBCC,0
.94:    DC.W    $5BC8,SIZE0,DBCC,0
.95:    DC.W    $5CC8,SIZE0,DBCC,0
.96:    DC.W    $5DC8,SIZE0,DBCC,0
.97:    DC.W    $5EC8,SIZE0,DBCC,0
.98:    DC.W    $5FC8,SIZE0,DBCC,0
.99:    DC.W    $52C0,SIZEO,1,SIZE_B,EA,DEST,0,0                                        ; Scc
.100:   DC.W    $53C0,SIZEO,1,SIZE_B,EA,DEST,0,0
.101:   DC.W    $54C0,SIZEO,1,SIZE_B,EA,DEST,0,0
.102:   DC.W    $55C0,SIZEO,1,SIZE_B,EA,DEST,0,0
.103:   DC.W    $56C0,SIZEO,1,SIZE_B,EA,DEST,0,0
.104:   DC.W    $57C0,SIZEO,1,SIZE_B,EA,DEST,0,0
.105:   DC.W    $58C0,SIZEO,1,SIZE_B,EA,DEST,0,0
.106:   DC.W    $59C0,SIZEO,1,SIZE_B,EA,DEST,0,0
.107:   DC.W    $5AC0,SIZEO,1,SIZE_B,EA,DEST,0,0
.108:   DC.W    $5BC0,SIZEO,1,SIZE_B,EA,DEST,0,0
.109:   DC.W    $5CC0,SIZEO,1,SIZE_B,EA,DEST,0,0
.110:   DC.W    $5DC0,SIZEO,1,SIZE_B,EA,DEST,0,0
.111:   DC.W    $5EC0,SIZEO,1,SIZE_B,EA,DEST,0,0
.112:   DC.W    $5FC0,SIZEO,1,SIZE_B,EA,DEST,0,0
.113:   DC.W    $50C0,SIZEO,1,SIZE_B,EA,DEST,0,0                                        ; ST
.114:   DC.W    $51C0,SIZEO,1,SIZE_B,EA,DEST,0,0                                        ; SF
.115:   DC.W    $0200,SIZE,2,SIZE_BWL,6,DOANDI,0                                        ; ANDI
.116:   DC.W    $0A00,SIZE,2,SIZE_BWL,6,DOEORI,0                                        ; EORI
.117:   DC.W    $4AFC,SIZE0,0                                                           ; ILLEGAL
.118:   DC.W    $4E74,SIZE0,CPUSUP,%11111110,EA,$800,-1,0                               ; RTD=118!!!
.119:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0018,0,FPUSUP,3,WARN46,0                 ; FABS
.120:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$001C,0,FPUSUP,3,WARN46,0                 ; FACOS
.121:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$000C,0,FPUSUP,3,WARN46,0                 ; FASIN
.122:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$000A,0,FPUSUP,3,WARN46,0                 ; FATAN
.123:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$000D,0,FPUSUP,3,WARN46,0                 ; FATANH
.124:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$001D,0,FPUSUP,3,WARN46,0                 ; FCOS
.125:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0019,0,FPUSUP,3,WARN46,0                 ; FCOSH
.126:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0010,0,FPUSUP,3,WARN46,0                 ; FETOX
.127:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0008,0,FPUSUP,3,WARN46,0                 ; FETOXM1
.128:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$001E,0,FPUSUP,3,WARN46,0                 ; FGETEXP
.129:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$001F,0,FPUSUP,3,WARN46,0                 ; FGETMAN
.130:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0001,0,FPUSUP,3,WARN46,0                 ; FINT
.131:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0003,0,FPUSUP,3,WARN46,0                 ; FINTRZ
.132:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0015,0,FPUSUP,3,WARN46,0                 ; FLOG10
.133:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0016,0,FPUSUP,3,WARN46,0                 ; FLOG2
.134:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0014,0,FPUSUP,3,WARN46,0                 ; FLOGN
.135:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0006,0,FPUSUP,3,WARN46,0                 ; FLOGNP1
.136:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$001A,0,FPUSUP,3,0                        ; FNEG
.137:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$000E,0,FPUSUP,3,WARN46,0                 ; FSIN
.138:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0002,0,FPUSUP,3,WARN46,0                 ; FSINH
.139:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0004,0,FPUSUP,3,0                        ; FSQRT
.140:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$000F,0,FPUSUP,3,WARN46,0                 ; FTAN
.141:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0009,0,FPUSUP,3,WARN46,0                 ; FTANH
.142:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0012,0,FPUSUP,3,WARN46,0                 ; FTWOTOX
.143:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPM,$0011,0,FPUSUP,3,WARN46,0                 ; FTWOTOX
.144:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPD,$0022,0,FPUSUP,3,0                        ; FADD
.145:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPD,$0038,0,FPUSUP,3,0                        ; FCMP
.146:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPD,$0020,0,FPUSUP,3,0                        ; FDIV
.147:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPD,$0021,0,FPUSUP,3,WARN46,0                 ; FMOD
.148:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPD,$0023,0,FPUSUP,3,0                        ; FMUL
.149:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPD,$0025,0,FPUSUP,3,WARN46,0                 ; FREM
.150:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPD,$0026,0,FPUSUP,3,WARN46,0                 ; FSCALE
.151:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPD,$0024,0,FPUSUP,3,WARN46,0                 ; FSGLDIV
.152:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPD,$0027,0,FPUSUP,3,WARN46,0                 ; FSGLMUL
.153:   DC.W    $F200,SIZEO,5,SIZE_BWLSXD,FPD,$0028,0,FPUSUP,3,0                        ; FSUB
.154:   DC.W    $F280,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2C0,0                              ; FBF
.155:   DC.W    $F281,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2C1,0
.156:   DC.W    $F282,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$f2C2,0
.157:   DC.W    $F283,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2C3,0
.158:   DC.W    $F284,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2C4,0
.159:   DC.W    $F285,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2C5,0
.160:   DC.W    $F286,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2C6,0
.161:   DC.W    $F287,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2C7,0
.162:   DC.W    $F288,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2C8,0
.163:   DC.W    $F289,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2C9,0
.164:   DC.W    $F28A,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2CA,0
.165:   DC.W    $F28B,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2CB,0
.166:   DC.W    $F28C,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2CC,0
.167:   DC.W    $F28D,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2CD,0
.168:   DC.W    $F28E,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2CE,0
.169:   DC.W    $F28F,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2CF,0
.170:   DC.W    $F290,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2D0,0
.171:   DC.W    $F291,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2D1,0
.172:   DC.W    $F292,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2D2,0
.173:   DC.W    $F293,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2D3,0
.174:   DC.W    $F294,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2D4,0
.175:   DC.W    $F295,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2D5,0
.176:   DC.W    $F296,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2D6,0
.177:   DC.W    $F297,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2D7,0
.178:   DC.W    $F298,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2D8,0
.179:   DC.W    $F299,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2D9,0
.180:   DC.W    $F29A,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2DA,0
.181:   DC.W    $F29B,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2DB,0
.182:   DC.W    $F29C,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2DC,0
.183:   DC.W    $F29D,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2DD,0
.184:   DC.W    $F29E,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2DE,0
.185:   DC.W    $F29F,SIZEO,2,SIZE_WL,FPUSUP,3,BCC,$F2DF,0
.186:   DC.W    $F2A0,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2E0,0
.187:   DC.W    $F2A1,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2E1,0
.188:   DC.W    $F2A2,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2E2,0
.189:   DC.W    $F2A3,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2E3,0
.190:   DC.W    $F2A4,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2E4,0
.191:   DC.W    $F2A5,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2E5,0
.192:   DC.W    $F2A6,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2E6,0
.193:   DC.W    $F2A7,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2E7,0
.194:   DC.W    $F2A8,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2E8,0
.195:   DC.W    $F2A9,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2E9,0
.196:   DC.W    $F2AA,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2EA,0
.197:   DC.W    $F2AB,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2EB,0
.198:   DC.W    $F2AC,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2EC,0
.199:   DC.W    $F2AD,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2ED,0
.200:   DC.W    $F2AE,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2EE,0
.201:   DC.W    $F2AF,SIZEO,2,SIZE_WL,MMUSUP,3,BCC,$F2EF,0
.202:   DC.W    $F200,SIZEO,4,SIZE_S,FPD,$0062,0,FPUSUP,2,0                             ; FSADD
.203:   DC.W    $F200,SIZEO,7,SIZE_D,FPD,$0066,0,FPUSUP,2,0                             ; FDADD
.204:   DC.W    $F200,SIZEO,4,SIZE_S,FPD,$0060,0,FPUSUP,2,0                             ; FSDIV
.205:   DC.W    $F200,SIZEO,7,SIZE_D,FPD,$0064,0,FPUSUP,2,0                             ; FDDIV
.206:   DC.W    $F200,SIZEO,4,SIZE_S,FPD,$0040,1,FPUSUP,2,0                             ; FSMOVE
.207:   DC.W    $F200,SIZEO,7,SIZE_D,FPD,$0044,1,FPUSUP,2,0                             ; FDMOVE
.208:   DC.W    $F200,SIZEO,4,SIZE_S,FPD,$0063,0,FPUSUP,2,0                             ; FSMUL
.209:   DC.W    $F200,SIZEO,7,SIZE_D,FPD,$0067,0,FPUSUP,2,0                             ; FDMUL
.210:   DC.W    $F200,SIZEO,4,SIZE_S,FPM,$005A,0,FPUSUP,2,0                             ; FSNEG
.211:   DC.W    $F200,SIZEO,7,SIZE_D,FPM,$005E,0,FPUSUP,2,0                             ; FDNEG
.212:   DC.W    $F200,SIZEO,4,SIZE_S,FPM,$0041,0,FPUSUP,2,0                             ; FSSQRT
.213:   DC.W    $F200,SIZEO,7,SIZE_D,FPM,$0045,0,FPUSUP,2,0                             ; FDSQRT
.214:   DC.W    $F200,SIZEO,4,SIZE_S,FPD,$0068,0,FPUSUP,2,0                             ; FSSUB
.215:   DC.W    $F200,SIZEO,7,SIZE_D,FPD,$006C,0,FPUSUP,2,0                             ; FDSUB
.216:   DC.W    $F24F,COPY,$0000,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0                  ; FDBF
.217:   DC.W    $F24F,COPY,$0001,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.218:   DC.W    $F24F,COPY,$0002,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.219:   DC.W    $F24F,COPY,$0003,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.220:   DC.W    $F24F,COPY,$0004,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.221:   DC.W    $F24F,COPY,$0005,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.222:   DC.W    $F24F,COPY,$0006,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.223:   DC.W    $F24F,COPY,$0007,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.224:   DC.W    $F24F,COPY,$0008,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.225:   DC.W    $F24F,COPY,$0009,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.226:   DC.W    $F24F,COPY,$000a,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.227:   DC.W    $F24F,COPY,$000b,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.228:   DC.W    $F24F,COPY,$000c,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.229:   DC.W    $F24F,COPY,$000d,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.230:   DC.W    $F24F,COPY,$000e,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.231:   DC.W    $F24F,COPY,$000f,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.232:   DC.W    $F24F,COPY,$0010,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.233:   DC.W    $F24F,COPY,$0011,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.234:   DC.W    $F24F,COPY,$0012,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.235:   DC.W    $F24F,COPY,$0013,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.236:   DC.W    $F24F,COPY,$0014,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.237:   DC.W    $F24F,COPY,$0015,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.238:   DC.W    $F24F,COPY,$0016,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.239:   DC.W    $F24F,COPY,$0017,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.240:   DC.W    $F24F,COPY,$0018,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.241:   DC.W    $F24F,COPY,$0019,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.242:   DC.W    $F24F,COPY,$001a,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.243:   DC.W    $F24F,COPY,$001b,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.244:   DC.W    $F24F,COPY,$001c,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.245:   DC.W    $F24F,COPY,$001d,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.246:   DC.W    $F24F,COPY,$001e,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.247:   DC.W    $F24F,COPY,$001f,SIZEO,2,SIZE_W,FPUSUP,3,WARN46,DBCC,0
.248:   DC.W    $F24F,COPY,$0020,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.249:   DC.W    $F24F,COPY,$0021,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.250:   DC.W    $F24F,COPY,$0022,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.251:   DC.W    $F24F,COPY,$0023,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.252:   DC.W    $F24F,COPY,$0024,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.253:   DC.W    $F24F,COPY,$0025,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.254:   DC.W    $F24F,COPY,$0026,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.255:   DC.W    $F24F,COPY,$0027,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.256:   DC.W    $F24F,COPY,$0028,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.257:   DC.W    $F24F,COPY,$0029,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.258:   DC.W    $F24F,COPY,$002a,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.259:   DC.W    $F24F,COPY,$002b,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.260:   DC.W    $F24F,COPY,$002c,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.261:   DC.W    $F24F,COPY,$002d,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.262:   DC.W    $F24F,COPY,$002e,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.263:   DC.W    $F24F,COPY,$002f,SIZEO,2,SIZE_W,MMUSUP,3,DBCC,0
.264:   DC.W    $F240,COPY,$0000,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0             ; FSF
.265:   DC.W    $F240,COPY,$0001,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.266:   DC.W    $F240,COPY,$0002,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.267:   DC.W    $F240,COPY,$0003,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.268:   DC.W    $F240,COPY,$0004,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.269:   DC.W    $F240,COPY,$0005,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.270:   DC.W    $F240,COPY,$0006,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.271:   DC.W    $F240,COPY,$0007,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.272:   DC.W    $F240,COPY,$0008,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.273:   DC.W    $F240,COPY,$0009,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.274:   DC.W    $F240,COPY,$000a,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.275:   DC.W    $F240,COPY,$000b,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.276:   DC.W    $F240,COPY,$000c,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.277:   DC.W    $F240,COPY,$000d,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.278:   DC.W    $F240,COPY,$000e,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.279:   DC.W    $F240,COPY,$000f,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.280:   DC.W    $F240,COPY,$0010,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.281:   DC.W    $F240,COPY,$0011,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.282:   DC.W    $F240,COPY,$0012,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.283:   DC.W    $F240,COPY,$0013,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.284:   DC.W    $F240,COPY,$0014,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.285:   DC.W    $F240,COPY,$0015,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.286:   DC.W    $F240,COPY,$0016,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.287:   DC.W    $F240,COPY,$0017,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.288:   DC.W    $F240,COPY,$0018,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.289:   DC.W    $F240,COPY,$0019,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.290:   DC.W    $F240,COPY,$001a,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.291:   DC.W    $F240,COPY,$001b,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.292:   DC.W    $F240,COPY,$001c,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.293:   DC.W    $F240,COPY,$001d,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.294:   DC.W    $F240,COPY,$001e,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.295:   DC.W    $F240,COPY,$001f,SIZEO,1,SIZE_B,EA,DEST,0,FPUSUP,3,WARN46,0
.296:   DC.W    $F240,COPY,$0020,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.297:   DC.W    $F240,COPY,$0021,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.298:   DC.W    $F240,COPY,$0022,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.299:   DC.W    $F240,COPY,$0023,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.300:   DC.W    $F240,COPY,$0024,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.301:   DC.W    $F240,COPY,$0025,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.302:   DC.W    $F240,COPY,$0026,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.303:   DC.W    $F240,COPY,$0027,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.304:   DC.W    $F240,COPY,$0028,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.305:   DC.W    $F240,COPY,$0029,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.306:   DC.W    $F240,COPY,$002a,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.307:   DC.W    $F240,COPY,$002b,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.308:   DC.W    $F240,COPY,$002c,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.309:   DC.W    $F240,COPY,$002d,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.310:   DC.W    $F240,COPY,$002e,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.311:   DC.W    $F240,COPY,$002f,SIZEO,1,SIZE_B,EA,DEST,0,MMUSUP,3,0
.312:   DC.W    $f200,COPY,$0000,SIZEO,5,SIZE_BWLSXPD,FPUSUP,3,FPMOVE,0                 ; FMOVE
.313:   DC.W    $f280,COPY,$0000,SIZE0,FPUSUP,3,0                                       ; FNOP
.314:   DC.W    $F200,COPY,$5C00,SIZEO,5,SIZE_X,FPUSUP,3,FMOVECR,WARN46,0               ; FMOVECR
.315:   DC.W    $F200,COPY,$0000,SIZEO,5,SIZE_LX,FPUSUP,3,FMOVEM,0                      ; FMOVEM
.316:   DC.W    $F340,SIZE0,EA,FRESTM,0,FPUSUP,3,0                                      ; FRESTORE
.317:   DC.W    $F300,SIZE0,EA,FSAVEM,0,FPUSUP,3,0                                      ; FSAVE
.318:   DC.W    $F200,COPY,$0030,SIZEO,5,SIZE_BWLSXD,FPUSUP,3,WARN46,FSINCOS,0          ; FSINCOS
.319:   DC.W    $F27a,COPY,$0000,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0                      ; FSF
.320:   DC.W    $F27a,COPY,$0001,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.321:   DC.W    $F27a,COPY,$0002,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.322:   DC.W    $F27a,COPY,$0003,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.323:   DC.W    $F27a,COPY,$0004,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.324:   DC.W    $F27a,COPY,$0005,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.325:   DC.W    $F27a,COPY,$0006,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.326:   DC.W    $F27a,COPY,$0007,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.327:   DC.W    $F27a,COPY,$0008,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.328:   DC.W    $F27a,COPY,$0009,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.329:   DC.W    $F27a,COPY,$000a,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.330:   DC.W    $F27a,COPY,$000b,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.331:   DC.W    $F27a,COPY,$000c,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.332:   DC.W    $F27a,COPY,$000d,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.333:   DC.W    $F27a,COPY,$000e,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.334:   DC.W    $F27a,COPY,$000f,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.335:   DC.W    $F27a,COPY,$0010,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.336:   DC.W    $F27a,COPY,$0011,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.337:   DC.W    $F27a,COPY,$0012,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.338:   DC.W    $F27a,COPY,$0013,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.339:   DC.W    $F27a,COPY,$0014,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.340:   DC.W    $F27a,COPY,$0015,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.341:   DC.W    $F27a,COPY,$0016,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.342:   DC.W    $F27a,COPY,$0017,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.343:   DC.W    $F27a,COPY,$0018,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.344:   DC.W    $F27a,COPY,$0019,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.345:   DC.W    $F27a,COPY,$001a,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.346:   DC.W    $F27a,COPY,$001b,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.347:   DC.W    $F27a,COPY,$001c,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.348:   DC.W    $F27a,COPY,$001d,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.349:   DC.W    $F27a,COPY,$001e,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.350:   DC.W    $F27a,COPY,$001f,SIZEO,2,SIZE_WL,TRAPCC,FPUSUP,3,0
.351:   DC.W    $F27a,COPY,$0020,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.352:   DC.W    $F27a,COPY,$0021,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.353:   DC.W    $F27a,COPY,$0022,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.354:   DC.W    $F27a,COPY,$0023,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.355:   DC.W    $F27a,COPY,$0024,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.356:   DC.W    $F27a,COPY,$0025,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.357:   DC.W    $F27a,COPY,$0026,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.358:   DC.W    $F27a,COPY,$0027,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.359:   DC.W    $F27a,COPY,$0028,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.360:   DC.W    $F27a,COPY,$0029,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.361:   DC.W    $F27a,COPY,$002a,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.362:   DC.W    $F27a,COPY,$002b,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.363:   DC.W    $F27a,COPY,$002c,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.364:   DC.W    $F27a,COPY,$002d,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.365:   DC.W    $F27a,COPY,$002e,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.366:   DC.W    $F27a,COPY,$002f,SIZEO,2,SIZE_WL,TRAPCC,MMUSUP,3,0
.367:   DC.W    $f200,COPY,$003a,SIZEO,5,SIZE_BWLSXD,FPUSUP,3,FTST,0                    ; FTST
.368:   DC.W    $F100,SIZE0,EA,FSAVEM,0,MMUSUP,3,0                                      ; PSAVE
.369:   DC.W    $F140,SIZE0,EA,FRESTM,0,MMUSUP,3,0                                      ; PRESTORE
.370:   DC.W    $F000,COPY,$2400,SIZE0,MMUSUP,3,0                                       ; PFLUSHA
.371:   DC.W    $F000,COPY,$3000,SIZE0,MMUSUP,3,PMMSTD,1,$f508,0                        ; PFLUSH
.372:   DC.W    $F000,COPY,$3400,SIZE0,MMUSUP,3,PMMSTD,0,0                              ; PFLUSHS
.373:   DC.W    $F000,COPY,$A000,SIZE0,MMUSUP,3,EA,PMDEST,0,0                           ; PFLUSHR
.374:   DC.W    $f000,COPY,$2200,SIZE0,MMUSUP,3,PMMLDX,0                                ; PLOADR
.375:   DC.W    $F000,COPY,$2000,SIZE0,MMUSUP,3,PMMLDX,0                                ; PLOADW
.376:   DC.W    $F000,COPY,$8200,SIZE0,MMUSUP,3,PMMTST,1,$f568,0                        ; PTESTR
.377:   DC.W    $F000,COPY,$8000,SIZE0,MMUSUP,3,PMMTST,1,$f548,0                        ; PTESTW
.378:   DC.W    $F000,COPY,$0000,SIZEO,8,SIZE_WLQ,PMMMOVE,0                             ; PMOVE
.379:   DC.W    $F510,SIZE0,MMUSUP,2,CPUSUP,%11000,0                                    ; PFLUSHAN
.380:   DC.W    $F500,SIZE0,MMUSUP,2,CPUSUP,%11000,EA,4,0,0                             ; PFLUSHN
.381:   DC.W    $F5C8,SIZE0,MMUSUP,2,CPUSUP,%10000,PMMEA,4,0,0                          ; PLPAR
.382:   DC.W    $F588,SIZE0,MMUSUP,2,CPUSUP,%10000,PMMEA,4,0,0                          ; PLPAW
.383:   DC.W    $4848,SIZE0,IMMBK,CPUSUP,%111111,0                                      ; BKPT
.384:   DC.W    $4E7A,COPY,0,SIZEO,3,SIZE_L,CPUSUP,%111111,MOVEC,0                      ; MOVEC
.385:   DC.W    $0E00,COPY,0,SIZEO,2,SIZE_BWL,CPUSUP,%111111,MOVES,0                    ; MOVES
.386:   DC.W    $EAC0,COPY,0,SIZE0,CPUSUP,%111110,BITFLD1,0                             ; BFCHG
.387:   DC.W    $ECC0,COPY,0,SIZE0,CPUSUP,%111110,BITFLD1,0                             ; BFCLR
.388:   DC.W    $EBC0,COPY,0,SIZE0,CPUSUP,%111110,BITFLD2,0                             ; BFEXTS
.389:   DC.W    $E9C0,COPY,0,SIZE0,CPUSUP,%111110,BITFLD2,0                             ; BFEXTU
.390:   DC.W    $EDC0,COPY,0,SIZE0,CPUSUP,%111110,BITFLD2,0                             ; BFFFO
.391:   DC.W    $EEC0,COPY,0,SIZE0,CPUSUP,%111110,BITFLD1,0                             ; BFSET
.392:   DC.W    $E8C0,COPY,0,SIZE0,CPUSUP,%111110,BITFLD1,0                             ; BFTST
.393:   DC.W    $EFC0,COPY,0,SIZE0,CPUSUP,%111110,BITFLD3,0                             ; BFINS
.394:   DC.W    $4C40,SIZEO,3,SIZE_L,CPUSUP,%111110,WARN46,MULDIV,$4C40,$0800,0         ; DIVSL
.395:   DC.W    $4C40,SIZEO,3,SIZE_L,CPUSUP,%111110,WARN46,MULDIV,$4C40,$0000,0         ; DIVUL
.396:   DC.W    $49C0,SIZEO,3,SIZE_L,CPUSUP,%111110,DATAREG,0,0                         ; EXTB
.397:   DC.W    $54FA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0                           ; TRAPCC
.398:   DC.W    $55FA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.399:   DC.W    $57FA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.400:   DC.W    $51FA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.401:   DC.W    $5CFA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.402:   DC.W    $5EFA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.403:   DC.W    $52FA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.404:   DC.W    $5FFA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.405:   DC.W    $53FA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.406:   DC.W    $5DFA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.407:   DC.W    $5BFA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.408:   DC.W    $56FA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.409:   DC.W    $5AFA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.410:   DC.W    $50FA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.411:   DC.W    $58FA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.412:   DC.W    $59FA,SIZEO,2,SIZE_WL,CPUSUP,%111110,TRAPCC,0
.413:   DC.W    $F448,SIZE0,CPUSUP,%111000,CPCACHE,1,0                                  ; CINVL
.414:   DC.W    $F450,SIZE0,CPUSUP,%111000,CPCACHE,1,0                                  ; CINVP
.415:   DC.W    $F458,SIZE0,CPUSUP,%111000,CPCACHE,0,0                                  ; CINVA
.416:   DC.W    $F468,SIZE0,CPUSUP,%111000,CPCACHE,1,0                                  ; CPUSHL
.417:   DC.W    $F470,SIZE0,CPUSUP,%111000,CPCACHE,1,0                                  ; CPUSHP
.418:   DC.W    $F478,SIZE0,CPUSUP,%111000,CPCACHE,0,0                                  ; CPUSHA
.419:   DC.W    $f610,SIZE0,CPUSUP,%111000,MOVE16,0                                     ; MOVE16
.420:   DC.W    $F800,COPY,$01C0,SIZE0,CPUSUP,%110000,LPSTOP,0                          ; LPSTOP
.421:   DC.W    $4ACB,SIZE0,CPUSUP,%110000,0                                            ; HALT
.422:   DC.W    $4ACC,SIZE0,CPUSUP,%110000,0                                            ; PULSE
;*-*
;; AssemblerKeywords
ASM_KWORDS:
        DC.L    .01,0,.02,0,.03,0,.04,0,.05,0,.06,0,.07,0,.08,0,.09,0,.10,0
        DC.L    .11,0,.12,0,.13,0,.14,0,.15,0,.16,0,.17,0,.18,0,.19,0,.20,0
        DC.L    .21,0,.22,0,.23,0,.24,0,.25,0,.26,0,.27,0,.28,0,.29,0,.30,0
        DC.L    .31,0,.32,0,.33,0,.34,0,.35,0,.36,0,.37,0,.38,0,.39,0,.40,0
        DC.L    .41,0,.42,0,.43,0,.44,0,.45,0,.46,0,.47,0,.48,0,.49,0,.50,0
        DC.L    .51,0,.52,0,.53,0,.54,0,.55,0,.56,0,.57,0,.58,0,.59,0,.60,0
        DC.L    .61,0,.62,0,.63,0,.64,0,.65,0,.66,0,.67,0,.68,0,.69,0,.70,0
        DC.L    .71,0,.72,0,.73,0,.74,0,.75,0,.76,0,.77,0,.78,0,.79,0,.80,0
        DC.L    .81,0,.82,0,.83,0,.84,0,.85,0,.86,0,.87,0,.88,0,.89,0,.90,0
        DC.L    .91,0,.92,0,.93,0,.94,0,.95,0,.96,0,.97,0,.98,0,.99,0

        DC.L    .100,0,.101,0,.102,0,.103,0,.104,0,.105,0,.106,0,.107,0,.108,0,.109,0
        DC.L    .110,0,.111,0,.112,0,.113,0,.114,0,.115,0,.116,0,.117,0,.118,0,.119,0
        DC.L    .120,0,.121,0,.122,0,.123,0,.124,0,.125,0,.126,0,.127,0,.128,0,.129,0
        DC.L    .130,0,.131,0,.132,0,.133,0,.134,0,.135,0,.136,0,.137,0,.138,0,.139,0

        DC.L    .140,0,.141,0,.142,0,.143,0,.144,0,.145,0,.146,0,.147,0,.148,0,.149,0
        DC.L    .150,0,.151,0,.152,0,.153,0,.154,0,.155,0,.156,0,.157,0,.158,0,.159,0
        DC.L    .160,0,.161,0,.162,0,.163,0,.164,0,.165,0,.166,0,.167,0,.168,0,.169,0
        DC.L    .170,0,.171,0,.172,0,.173,0,.174,0;,.175,0,.176,0,.177,0,.178,0,.179,0

        DC.L    0,0


.01:    DC.B    "SFC",0
.02:    DC.B    "DFC",0
.03:    DC.B    "CACR",0
.04:    DC.B    "TC",0
.05:    DC.B    "ITT0",0
.06:    DC.B    "ITT1",0
.07:    DC.B    "DTT0",0
.08:    DC.B    "DTT1",0
.09:    DC.B    "BUSCR",0
.10:    DC.B    "USP",0
.11:    DC.B    "VBR",0
.12:    DC.B    "CAAR",0
.13:    DC.B    "MSP",0
.14:    DC.B    "ISP",0
.15:    DC.B    "MMUSR",0
.16:    DC.B    "URP",0
.17:    DC.B    "SRP",0
.18:    DC.B    "PCR",0
.19:    DC.B    "CRP",0
.20:    DC.B    "TT0",0
.21:    DC.B    "TT1",0
.22:    DC.B    "BAD0",0
.23:    DC.B    "BAC0",0
.24:    DC.B    "PCSR",0
.25:    DC.B    "CAL",0
.26:    DC.B    "VAL",0
.27:    DC.B    "SCC",0
.28:    DC.B    "DRP",0
.29:    DC.B    "FP0",0
.30:    DC.B    "FP1",0
.31:    DC.B    "FP2",0
.32:    DC.B    "FP3",0
.33:    DC.B    "FP4",0
.34:    DC.B    "FP5",0
.35:    DC.B    "FP6",0
.36:    DC.B    "FP7",0
.37:    DC.B    "FPSR",0
.38:    DC.B    "FPCR",0
.39:    DC.B    "FPIAR",0
.40:    DC.B    "CCR",0
.41:    DC.B    "SR",0
.42:    DC.B    "BAD1",0
.43:    DC.B    "BAD2",0
.44:    DC.B    "BAD3",0
.45:    DC.B    "BAD4",0
.46:    DC.B    "BAD5",0
.47:    DC.B    "BAD6",0
.48:    DC.B    "BAD7",0
.49:    DC.B    "BAC1",0
.50:    DC.B    "BAC2",0
.51:    DC.B    "BAC3",0
.52:    DC.B    "BAC4",0
.53:    DC.B    "BAC5",0
.54:    DC.B    "BAC6",0
.55:    DC.B    "BAC7",0
.56:    DC.B    "DC",0
.57:    DC.B    "IC",0
.58:    DC.B    "BC",0
.59:    DC.B    "NC",0

.60:    DC.B    "R0",0
.61:    DC.B    "R1",0
.62:    DC.B    "R2",0
.63:    DC.B    "R3",0
.64:    DC.B    "R4",0
.65:    DC.B    "R5",0
.66:    DC.B    "R6",0
.67:    DC.B    "R7",0
.68:    DC.B    "R8",0
.69:    DC.B    "R9",0
.70:    DC.B    "R10",0
.71:    DC.B    "R11",0
.72:    DC.B    "R12",0
.73:    DC.B    "R13",0
.74:    DC.B    "R14",0
.75:    DC.B    "R15",0
.76:    DC.B    "R16",0
.77:    DC.B    "R17",0
.78:    DC.B    "R18",0
.79:    DC.B    "R19",0
.80:    DC.B    "R20",0
.81:    DC.B    "R21",0
.82:    DC.B    "R22",0
.83:    DC.B    "R23",0
.84:    DC.B    "R24",0
.85:    DC.B    "R25",0
.86:    DC.B    "R26",0
.87:    DC.B    "R27",0
.88:    DC.B    "R28",0
.89:    DC.B    "R29",0
.90:    DC.B    "R30",0
.91:    DC.B    "R31",0

.92:    Dc.B    "CR0",0
.93:    Dc.B    "CR1",0
.94:    Dc.B    "CR2",0
.95:    Dc.B    "CR3",0
.96:    Dc.B    "CR4",0
.97:    Dc.B    "CR5",0
.98:    Dc.B    "CR6",0
.99:    Dc.B    "CR7",0

.100:   DC.B    'F0',0
.101:   DC.B    'F1',0
.102:   DC.B    'F2',0
.103:   DC.B    'F3',0
.104:   DC.B    'F4',0
.105:   DC.B    'F5',0
.106:   DC.B    'F6',0
.107:   DC.B    'F7',0
.108:   DC.B    'F8',0
.109:   DC.B    'F9',0
.110:   DC.B    'F10',0
.111:   DC.B    'F11',0
.112:   DC.B    'F12',0
.113:   DC.B    'F13',0
.114:   DC.B    'F14',0
.115:   DC.B    'F15',0
.116:   DC.B    'F16',0
.117:   DC.B    'F17',0
.118:   DC.B    'F18',0
.119:   DC.B    'F19',0
.120:   DC.B    'F20',0
.121:   DC.B    'F21',0
.122:   DC.B    'F22',0
.123:   DC.B    'F23',0
.124:   DC.B    'F24',0
.125:   DC.B    'F25',0
.126:   DC.B    'F26',0
.127:   DC.B    'F27',0
.128:   DC.B    'F28',0
.129:   DC.B    'F29',0
.130:   DC.B    'F30',0
.131:   DC.B    'F31',0

.132:   Dc.B    "CRF0",0
.133:   Dc.B    "CRF1",0
.134:   Dc.B    "CRF2",0
.135:   Dc.B    "CRF3",0
.136:   Dc.B    "CRF4",0
.137:   Dc.B    "CRF5",0
.138:   Dc.B    "CRF6",0
.139:   Dc.B    "CRF7",0

.140:   DC.B    'XER',0
.141:   DC.B    'LR',0
.142:   DC.B    'CTR',0
.143:   DC.B    'DSISR',0
.144:   DC.B    'DAR',0
.145:   DC.B    'DEC',0
.146:   DC.B    'SDR1',0
.147:   DC.B    'SRR0',0
.148:   DC.B    'SRR1',0
.149:   DC.B    'SPRG0',0
.150:   DC.B    'SPRG1',0
.151:   DC.B    'SPRG2',0
.152:   DC.B    'SPRG3',0
.153:   DC.B    0,0
.154:   DC.B    'EAR',0
.155:   DC.B    'PVR',0
.156:   DC.B    'IBAT0U',0
.157:   DC.B    'IBAT0L',0
.158:   DC.B    'IBAT1U',0
.159:   DC.B    'IBAT1L',0
.160:   DC.B    'IBAT2U',0
.161:   Dc.B    'IBAT2L',0
.162:   DC.B    'IBAT3U',0
.163:   DC.B    'IBAT3L',0
.164:   DC.B    'DBAT0U',0
.165:   DC.B    'DBAT0L',0
.166:   DC.B    'DBAT1U',0
.167:   DC.B    'DBAT1L',0
.168:   DC.B    'DBAT2U',0
.169:   DC.B    'DBAT2L',0
.170:   DC.B    'DBAT3U',0
.171:   DC.B    'DBAT3L',0
.172:   DC.B    'DABR',0

.173:   DC.B    'TBL',0
.174:   DC.B    'TBU',0

    EVEN
;*-*

;; PowePC assembler instruction set
PPC_INSTAB:
    DC.L    .01,0,.02,0,.03,0,.04,0,.05,0,.06,0,.07,0,.08,0
    DC.L    .09,0,.0A,0,.0B,0,.0C,0,.0D,0,.0E,0,.0F,0,.10,0
    DC.L    .11,0,.12,0,.13,0,.14,0,.15,0,.16,0,.17,0,.18,0
    DC.L    .19,0,.1A,0,.1B,0,.1C,0,.1D,0,.1E,0,.1F,0,.20,0
    DC.L    .21,0,.22,0,.23,0,.24,0,.25,0,.26,0,.27,0,.28,0
    DC.L    .29,0,.2A,0,.2B,0,.2C,0,.2D,0,.2E,0,.2F,0,.30,0
    DC.L    .31,0,.32,0,.33,0,.34,0,.35,0,.36,0,.37,0,.38,0
    DC.L    .39,0,.3A,0,.3B,0,.3C,0,.3D,0,.3E,0,.3F,0,.40,0
    DC.L    .41,0,.42,0,.43,0,.44,0,.45,0,.46,0,.47,0,.48,0
    DC.L    .49,0,.4A,0,.4B,0,.4C,0,.4D,0,.4E,0,.4F,0,.50,0
    DC.L    .51,0,.52,0,.53,0,.54,0,.55,0,.56,0,.57,0,.58,0
    DC.L    .59,0,.5A,0,.5B,0,.5C,0,.5D,0,.5E,0,.5F,0,.60,0
    DC.L    .61,0,.62,0,.63,0,.64,0,.65,0,.66,0,.67,0,.68,0
    DC.L    .69,0,.6A,0,.6B,0,.6C,0,.6D,0,.6E,0,.6F,0,.70,0
    DC.L    .71,0,.72,0,.73,0,.74,0,.75,0,.76,0,.77,0,.78,0
    DC.L    .79,0,.7A,0,.7B,0,.7C,0,.7D,0,.7E,0,.7F,0,.80,0
    DC.L    .81,0,.82,0,.83,0,.84,0,.85,0,.86,0,.87,0,.88,0
    DC.L    .89,0,.8A,0,.8B,0,.8C,0,.8D,0,.8E,0,.8F,0,.90,0
    DC.L    .91,0,.92,0,.93,0,.94,0,.95,0,.96,0,.97,0,.98,0
    DC.L    .99,0,.9A,0,.9B,0,.9C,0,.9D,0,.9E,0,.9F,0,.A0,0
    DC.L    .A1,0,.A2,0,.A3,0,.A4,0,.A5,0,.A6,0,.A7,0,.A8,0
    DC.L    .A9,0,.AA,0,.AB,0,.AC,0,.AD,0,.AE,0,.AF,0,.B0,0
    DC.L    .B1,0,.B2,0,.B3,0,.B4,0,.B5,0,.B6,0,.B7,0,.B8,0
    DC.L    .B9,0,.BA,0,.BB,0,.BC,0,.BD,0,.BE,0,.BF,0,.C0,0
    DC.L    .C1,0,.C2,0,.C3,0,.C4,0,.C5,0,.C6,0,.C7,0,.C8,0
    DC.L    .C9,0,.CA,0,.CB,0,.CC,0,.CD,0,.CE,0,.CF,0,.D0,0
    DC.L    .D1,0,.D2,0,.D3,0,.D4,0,.D5,0,.D6,0,.D7,0,.D8,0
    DC.L    .D9,0,.DA,0,.DB,0,.DC,0,.DD,0,.DE,0,.DF,0,.E0,0
    DC.L    .E1,0,.E2,0,.E3,0,.E4,0,.E5,0,.E6,0,.E7,0,.E8,0
    DC.L    .E9,0,.EA,0,.EB,0,.EC,0,.ED,0,.EE,0,.EF,0,.F0,0
    DC.L    .F1,0,.F2,0,.F3,0,.F4,0,.F5,0,.F6,0,.F7,0,.F8,0
    DC.L    .F9,0,.FA,0,.FB,0,.FC,0,.FD,0,.FE,0,.FF,0,.G0,0
    DC.L    .G1,0,.G2,0,.G3,0,.G4,0,.G5,0,.G6,0,.G7,0,.G8,0
    DC.L    .G9,0,.GA,0,.GB,0,.GC,0,.GD,0,.GE,0,.GF,0


    DC.L    0,0
.01:DC.B    'ADD',0
.02:DC.B    'ADDO',0
.03:DC.B    'ADDC',0
.04:DC.B    'ADDCO',0
.05:DC.B    'ADDE',0
.06:DC.B    'ADDEO',0
.07:DC.B    'ADDI',0
.08:DC.B    'ADDIC',0
.09:DC.B    'ADDIS',0
.0A:DC.B    'ADDME',0
.0B:DC.B    'ADDMEO',0
.0C:DC.B    'ADDZE',0
.0D:DC.B    'ADDZEO',0
.0E:DC.B    'AND',0
.0F:DC.B    'ANDC',0
.10:DC.B    'ANDI',0
.11:DC.B    'ANDIS',0
.12:DC.B    'B',0
.13:DC.B    'BA',0
.14:DC.B    'BL',0
.15:DC.B    'BLA',0
.16:DC.B    'BC',0
.17:DC.B    'BCA',0
.18:DC.B    'BCL',0
.19:DC.B    'BCLA',0
.1A:DC.B    'BCCTR',0
.1B:DC.B    'BCCTRL',0
.1C:DC.B    'BCLR',0
.1D:DC.B    'BCLRL',0
.1E:DC.B    'CMP',0
.1F:DC.B    'CMPD',0
.20:DC.B    'CMPW',0
.21:DC.B    'CMPI',0
.22:DC.B    'CMPDI',0
.23:DC.B    'CMPWI',0
.24:DC.B    'CMPL',0
.25:DC.B    'CMPLD',0
.26:DC.B    'CMPLW',0
.27:DC.B    'CMPLI',0
.28:DC.B    'CMPLDI',0
.29:DC.B    'CMPLWI',0
.2A:DC.B    'CNTLZD',0
.2B:DC.B    'CNTLZW',0
.2C:DC.B    'CRAND',0
.2D:DC.B    'CRANDC',0
.2E:Dc.B    'CREQV',0
.2F:DC.B    'CRNAND',0
.30:DC.B    'CRNOR',0
.31:DC.B    'CRNOT',0
.32:DC.B    'CROR',0
.33:DC.B    'CRMOVE',0
.34:DC.B    'CRORC',0
.35:DC.B    'CRXOR',0
.36:DC.B    'DCBA',0
.37:DC.B    'DCBF',0
.38:DC.B    'DCBI',0
.39:DC.B    'DCBST',0
.3A:DC.B    'DCBT',0
.3B:DC.B    'DCBTST',0
.3C:DC.B    'DCBZ',0
.3D:DC.B    'DIVD',0
.3E:DC.B    'DIVDO',0
.3F:DC.B    'DIVDU',0
.40:DC.B    'DIVDUO',0
.41:DC.B    'DIVW',0
.42:Dc.B    'DIVWO',0
.43:DC.B    'DIVWU',0
.44:DC.B    'DIVWUO',0
.45:DC.B    'ECIWX',0
.46:DC.B    'ECOWX',0
.47:DC.B    'EIEIO',0
.48:DC.B    'EQV',0
.49:DC.B    'EXTSB',0
.4A:DC.B    'EXTSH',0
.4B:DC.B    'EXTSW',0
.4C:DC.B    'FABS',0
.4D:DC.B    'FADD',0
.4E:DC.B    'FSADD',0
.4F:DC.B    'FCFID',0
.50:DC.B    'FCMPO',0
.51:DC.B    'FCMPU',0
.52:DC.B    'FCTID',0
.53:DC.B    'FCTIDZ',0
.54:DC.B    'FCTIW',0
.55:DC.B    'FCTIWZ',0
.56:DC.B    'FDIV',0
.57:DC.B    'FDIVS',0
.58:DC.B    'FMADD',0
.59:DC.B    'FMADDS',0
.5A:DC.B    'FMR',0
.5B:DC.B    'FMSUB',0
.5C:DC.B    'FMSUBS',0
.5D:DC.B    'FMUL',0
.5E:DC.B    'FMULS',0
.5F:DC.B    'FNABS',0
.60:DC.B    'FNEG',0
.61:DC.B    'FNMADD',0
.62:DC.B    'FNMADDS',0
.63:DC.B    'FNMSUB',0
.64:DC.B    'FNMSUBS',0
.65:DC.B    'FRES',0
.66:DC.B    'FRSP',0
.67:DC.B    'FRSQRTE',0
.68:DC.B    'FSEL',0
.69:DC.B    'FSQRT',0
.6A:DC.B    'FSQRTS',0
.6B:DC.B    'FSUB',0
.6C:DC.B    'FSUBS',0
.6D:DC.B    'ICBI',0
.6E:DC.B    'ISYNC',0
.6F:DC.B    'LBZ',0
.70:DC.B    'LBZU',0
.71:DC.B    'LBZUX',0
.72:DC.B    'LBZX',0
.73:DC.B    'LD',0
.74:DC.B    'LDARX',0
.75:DC.B    'LDU',0
.76:DC.B    'LDUX',0
.77:DC.B    'LDX',0
.78:DC.B    'LFD',0
.79:DC.B    'LFDU',0
.7A:DC.B    'LFDUX',0
.7B:DC.B    'LFDX',0
.7C:DC.B    'LFS',0
.7D:DC.B    'LFSU',0
.7E:DC.B    'LFSUX',0
.7F:DC.B    'LFSX',0
.80:DC.B    'LHA',0
.81:DC.B    'LHAU',0
.82:DC.B    'LHAUX',0
.83:DC.B    'LHAX',0
.84:DC.B    'LHBRX',0
.85:DC.B    'LHZ',0
.86:DC.B    'LHZU',0
.87:DC.B    'LHZUX',0
.88:DC.B    'LHZX',0
.89:DC.B    'LMW',0
.8A:DC.B    'LSWI',0
.8B:DC.B    'LSWX',0
.8C:DC.B    'LWA',0
.8D:DC.B    'LWARX',0
.8E:DC.B    'LWAUX',0
.8F:DC.B    'LWAX',0
.90:DC.B    'LWBRX',0
.91:DC.B    'LWZ',0
.92:DC.B    'LWZU',0
.93:DC.B    'LWZUX',0
.94:DC.B    'LWZX',0
.95:DC.B    'MCRF',0
.96:DC.B    'MCRFS',0
.97:DC.B    'MCRXR',0
.98:DC.B    'MFCR',0
.99:DC.B    'MFFS',0
.9A:DC.B    'MFMSR',0
.9B:DC.B    'MFSPR',0
.9C:DC.B    'MFXER',0
.9D:DC.B    'MFLR',0
.9E:DC.B    'MFCTR',0
.9F:DC.B    'MFSR',0
.A0:DC.B    'MFSRIN',0
.A1:DC.B    'MFTB',0
.A2:DC.B    'MFTBU',0
.A3:DC.B    'MTCRF',0
.A4:DC.B    'MTCR',0
.A5:DC.B    'MTFSB0',0
.A6:DC.B    'MTFSB1',0
.A7:DC.B    'MTFSF',0
.A8:DC.B    'MTFSFI',0
.A9:DC.B    'MTMSR',0
.AA:DC.B    'MTMSRD',0
.AB:DC.B    'MTSPR',0
.AC:DC.B    'MTSR',0
.AD:DC.B    'MTSRD',0
.AE:DC.B    'MTSRDIN',0
.AF:DC.B    'MTSRIN',0
.B0:DC.B    'MULHD',0
.B1:DC.B    'MULHDU',0
.B2:DC.B    'MULHW',0
.B3:DC.B    'MULHWU',0
.B4:DC.B    'MULLD',0
.B5:DC.B    'MULLDO',0
.B6:DC.B    'MULLI',0
.B7:DC.B    'MULLW',0
.B8:DC.B    'MULLWO',0
.B9:DC.B    'NAND',0
.BA:DC.B    'NEG',0
.BB:DC.B    'NEGO',0
.BC:DC.B    'NOR',0
.BD:DC.B    'OR',0
.BE:DC.B    'MR',0
.BF:DC.B    'ORC',0
.C0:DC.B    'ORI',0
.C1:DC.B    'NOP',0
.C2:DC.B    'ORIS',0
.C3:DC.B    'RFI',0
.C4:DC.B    'RFID',0
.C5:DC.B    'RLDCL',0
.C6:DC.B    'ROTLD',0
.C7:DC.B    'RLDCR',0
.C8:DC.B    'RLDIC',0
.C9:DC.B    'RLDICL',0
.CA:DC.B    'RLDICR',0
.CB:DC.B    'RLDIMI',0
.CC:DC.B    'RLWIMI',0
.CD:DC.B    'RLWINM',0
.CE:DC.B    'RLWNM',0
.CF:DC.B    'SC',0
.D0:DC.B    'SLBIA',0
.D1:DC.B    'SLBIE',0
.D2:DC.B    'SLD',0
.D3:DC.B    'SLW',0
.D4:DC.B    'SRAD',0
.D5:DC.B    'SRADI',0
.D6:DC.B    'SRAW',0
.D7:DC.B    'SRAWI',0
.D8:DC.B    'SRD',0
.D9:DC.B    'SRW',0
.DA:DC.B    'STB',0
.DB:DC.B    'STBU',0
.DC:DC.B    'STBUX',0
.DD:DC.B    'STBX',0
.DE:DC.B    'STD',0
.DF:DC.B    'STDCX',0
.E0:DC.B    'STDU',0
.E1:DC.B    'STDUX',0
.E2:DC.B    'STDX',0
.E3:DC.B    'STFD',0
.E4:DC.B    'STFDU',0
.E5:DC.B    'STFDUX',0
.E6:DC.B    'STFDX',0
.E7:DC.B    'STFIWX',0
.E8:DC.B    'STFS',0
.E9:DC.B    'STFSU',0
.EA:DC.B    'STFSUX',0
.EB:DC.B    'STFSX',0
.EC:DC.B    'STH',0
.ED:DC.B    'STHBRX',0
.EE:DC.B    'STHU',0
.EF:DC.B    'STHUX',0
.F0:DC.B    'STHX',0
.F1:DC.B    'STMW',0
.F2:DC.B    'STSWI',0
.F3:DC.B    'STSWX',0
.F4:DC.B    'STW',0
.F5:DC.B    'STWBRX',0
.F6:DC.B    'STWCX',0
.F7:DC.B    'STWU',0
.F8:DC.B    'STWUX',0
.F9:DC.B    'STWX',0
.FA:DC.B    'SUBF',0
.FB:DC.B    'SUBFO',0
.FC:DC.B    'SUBFC',0
.FD:DC.B    'SUBFCO',0
.FE:DC.B    'SUBFE',0
.FF:DC.B    'SUBFEO',0
.G0:DC.B    'SUBFIC',0
.G1:DC.B    'SUBFME',0
.G2:DC.B    'SUBFMEO',0
.G3:DC.B    'SUBFZE',0
.G4:DC.B    'SUBFZEO',0
.G5:DC.B    'SYNC',0
.G6:DC.B    'TD',0
.G7:DC.B    'TDI',0
.G8:DC.B    'TLBIA',0
.G9:DC.B    'TLBIE',0
.GA:DC.B    'TBLSYNC',0
.GB:DC.B    'TW',0
.GC:DC.B    'TWI',0
.GD:DC.B    'XOR',0
.GE:DC.B    'XORI',0
.GF:DC.B    'XORIS',0
    EVEN
;*-*
;; PowerPC assembler job set

PPC_DONE=0
PPC_CODE=1      ; num entries, entry, shift, entry, shift..
PPC_COMMA=2     ;
PPC_ASKGPR=3    ; shift
PPC_SETRC=4     ; bit nr. (def. 31)
PPC_DORC=5      ; bit nr.
PPC_SIMM=6      ; shift
PPC_DISRC=7     ;
PPC_GPRZERO=8   ; shift
PPC_REQRC=9     ;
PPC_NOTIMPL=10  ;
PPC_GET5BIT=11  ; shift
PPC_GETCRD=12   ; shift
PPC_GETBIT=13   ; shift
PPC_GETDBL5=14  ; shift,shift
PPC_ASKFPR=15   ; shift
PPC_GETCRFD=16  ; shift
PPC_RELATIVE=17 ; shift,mask,shift
PPC_ASKSPR=18   ; shift
PPC_GET4BIT=19  ; shift
PPC_GETTB=20    ; shift
PPC_GETXBIT=21  ; bits, shift
PPC_DBLGPR=22   ; shift,shift
PPC_SPLIT6B=23  ; shift,shift
PPC_LABEL=24    ;
PPC_ABSOLUTE=25 ;
PPC_LAB16=26    ;
PPC_ABS16=27    ;

PPC_INSJOB:
    DC.L    .01,.02,.03,.04,.05,.06,.07,.08,.09,.0A,.0B,.0C,.0D,.0E,.0F,.10
    DC.L    .11,.12,.13,.14,.15,.16,.17,.18,.19,.1A,.1B,.1C,.1D,.1E,.1F,.20
    DC.L    .21,.22,.23,.24,.25,.26,.27,.28,.29,.2A,.2B,.2C,.2D,.2E,.2F,.30
    DC.L    .31,.32,.33,.34,.35,.36,.37,.38,.39,.3A,.3B,.3C,.3D,.3E,.3F,.40
    DC.L    .41,.42,.43,.44,.45,.46,.47,.48,.49,.4A,.4B,.4C,.4D,.4E,.4F,.50
    DC.L    .51,.52,.53,.54,.55,.56,.57,.58,.59,.5A,.5B,.5C,.5D,.5E,.5F,.60
    DC.L    .61,.62,.63,.64,.65,.66,.67,.68,.69,.6A,.6B,.6C,.6D,.6E,.6F,.70
    DC.L    .71,.72,.73,.74,.75,.76,.77,.78,.79,.7A,.7B,.7C,.7D,.7E,.7F,.80
    DC.L    .81,.82,.83,.84,.85,.86,.87,.88,.89,.8A,.8B,.8C,.8D,.8E,.8F,.90
    DC.L    .91,.92,.93,.94,.95,.96,.97,.98,.99,.9A,.9B,.9C,.9D,.9E,.9F,.A0
    DC.L    .A1,.A2,.A3,.A4,.A5,.A6,.A7,.A8,.A9,.AA,.AB,.AC,.AD,.AE,.AF,.B0
    DC.L    .B1,.B2,.B3,.B4,.B5,.B6,.B7,.B8,.B9,.BA,.BB,.BC,.BD,.BE,.BF,.C0
    DC.L    .C1,.C2,.C3,.C4,.C5,.C6,.C7,.C8,.C9,.CA,.CB,.CC,.CD,.CE,.CF,.D0
    DC.L    .D1,.D2,.D3,.D4,.D5,.D6,.D7,.D8,.D9,.DA,.DB,.DC,.DD,.DE,.DF,.E0
    DC.L    .E1,.E2,.E3,.E4,.E5,.E6,.E7,.E8,.E9,.EA,.EB,.EC,.ED,.EE,.EF,.F0
    DC.L    .F1,.F2,.F3,.F4,.F5,.F6,.F7,.F8,.F9,.FA,.FB,.FC,.FD,.FE,.FF,.G0
    DC.L    .G1,.G2,.G3,.G4,.G5,.G6,.G7,.G8,.G9,.GA,.GB,.GC,.GD,.GE,.GF
    DC.L    00
.00:DC.W    0
.01:DC.W    PPC_CODE,2,31,5,266,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,PPC_DONE
.02:DC.W    PPC_CODE,3,31,5,1,21,266,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,PPC_DONE
.03:DC.W    PPC_CODE,2,31,5,10,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,PPC_DONE
.04:DC.W    PPC_CODE,3,31,5,1,21,10,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,PPC_DONE
.05:DC.W    PPC_CODE,2,31,5,138,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,PPC_DONE
.06:DC.W    PPC_CODE,3,31,5,1,21,138,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,PPC_DONE
.07:DC.W    PPC_CODE,1,14,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_SIMM,31,0
.08:DC.W    PPC_CODE,1,12,5,PPC_SETRC,5,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.09:DC.W    PPC_CODE,1,15,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.0A:DC.W    PPC_CODE,2,31,5,234,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,0
.0B:DC.W    PPC_CODE,3,31,5,1,21,234,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,0
.0C:DC.W    PPC_CODE,2,31,5,202,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,0
.0D:DC.W    PPC_CODE,3,31,5,202,30,1,21,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,0
.0E:DC.W    PPC_CODE,2,31,5,28,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.0F:DC.W    PPC_CODE,2,31,5,60,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.10:DC.W    PPC_CODE,1,28,5,PPC_REQRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_SIMM,31,0
.11:DC.W    PPC_CODE,1,29,5,PPC_REQRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_SIMM,31,0
.12:DC.W    PPC_CODE,1,18,5,PPC_DISRC,PPC_LABEL,0
.13:DC.W    PPC_CODE,2,18,5,1,30,PPC_DISRC,PPC_ABSOLUTE,0
.14:DC.W    PPC_CODE,2,18,5,1,31,PPC_DISRC,PPC_LABEL,0
.15:DC.W    PPC_CODE,2,18,5,3,31,PPC_DISRC,PPC_ABSOLUTE,0
.16:DC.W    PPC_CODE,1,16,5,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_LAB16,0
.17:DC.W    PPC_CODE,2,16,5,1,30,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_ABS16,0
.18:DC.W    PPC_CODE,2,16,5,1,31,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_LAB16,0
.19:DC.W    PPC_CODE,2,16,5,3,31,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_ABS16,0
.1A:DC.W    PPC_CODE,2,19,5,528,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,0
.1B:DC.W    PPC_CODE,3,19,5,528,30,1,31,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,0
.1C:DC.W    PPC_CODE,2,19,5,16,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,0
.1D:DC.W    PPC_CODE,3,19,5,16,30,1,31,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,0
.1E:DC.W    PPC_CODE,1,31,5,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_GETBIT,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.1F:DC.W    PPC_CODE,2,31,5,1,10,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.20:DC.W    PPC_CODE,1,31,5,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.21:DC.W    PPC_CODE,1,11,5,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_GETBIT,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.22:DC.W    PPC_CODE,2,11,5,1,10,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.23:DC.W    PPC_CODE,1,11,5,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.24:DC.W    PPC_CODE,2,31,5,32,30,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_GETBIT,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.25:DC.W    PPC_CODE,3,31,5,1,10,32,30,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.26:DC.W    PPC_CODE,2,31,5,32,30,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.27:DC.W    PPC_CODE,1,10,5,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_GETBIT,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.28:DC.W    PPC_CODE,2,10,5,1,10,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.29:DC.W    PPC_CODE,1,10,5,PPC_DISRC,PPC_GETCRD,8,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.2A:DC.W    PPC_CODE,2,31,5,58,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,0
.2B:DC.W    PPC_CODE,2,31,5,26,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,0
.2C:DC.W    PPC_CODE,2,19,5,257,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_GET5BIT,20,0
.2D:DC.W    PPC_CODE,2,19,5,129,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_GET5BIT,20,0
.2E:DC.W    PPC_CODE,2,19,5,289,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_GET5BIT,20,0
.2F:DC.W    PPC_CODE,2,19,5,225,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_GET5BIT,20,0
.30:DC.W    PPC_CODE,2,19,5,33,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_GET5BIT,20,0
.31:DC.W    PPC_CODE,2,19,5,33,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GETDBL5,15,20,0
.32:DC.W    PPC_CODE,2,19,5,449,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_GET5BIT,20,0
.33:DC.W    PPC_CODE,2,19,5,449,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GETDBL5,15,20,0
.34:DC.W    PPC_CODE,2,19,5,417,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_GET5BIT,20,0
.35:DC.W    PPC_CODE,2,19,5,193,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_GET5BIT,15,PPC_COMMA,PPC_GET5BIT,20,0
.36:DC.W    PPC_CODE,2,31,5,758,30,PPC_DISRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.37:DC.W    PPC_CODE,2,31,5,86,30,PPC_DISRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.38:DC.W    PPC_CODE,2,31,5,470,30,PPC_DISRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.39:DC.W    PPC_CODE,2,31,5,54,30,PPC_DISRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.3A:DC.W    PPC_CODE,2,31,278,30,PPC_DISRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.3B:DC.W    PPC_CODE,2,31,5,246,30,PPC_DISRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.3C:DC.W    PPC_CODE,2,31,5,1014,30,PPC_DISRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.3D:DC.W    PPC_CODE,2,31,5,489,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.3E:DC.W    PPC_CODE,3,31,5,1,21,489,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.3F:DC.W    PPC_CODE,2,31,5,457,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.40:DC.W    PPC_CODE,3,31,5,1,21,457,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.41:DC.W    PPC_CODE,2,31,5,491,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.42:DC.W    PPC_CODE,3,31,5,1,21,491,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.43:DC.W    PPC_CODE,2,31,5,459,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.44:DC.W    PPC_CODE,3,31,5,1,21,459,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.45:DC.W    PPC_CODE,2,31,5,310,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.46:DC.W    PPC_CODE,2,31,5,438,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.47:DC.W    PPC_CODE,2,31,5,854,30,0
.48:DC.W    PPC_CODE,2,31,5,284,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.49:DC.W    PPC_CODE,2,31,5,954,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,0
.4A:DC.W    PPC_CODE,2,31,5,922,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,0
.4B:DC.W    PPC_CODE,2,31,5,986,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,0
.4C:DC.W    PPC_CODE,2,63,5,264,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.4D:DC.W    PPC_CODE,2,63,5,21,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,20,0
.4E:DC.W    PPC_CODE,2,59,5,21,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,20,0
.4F:DC.W    PPC_CODE,2,63,5,846,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.50:DC.W    PPC_CODE,2,63,5,32,30,PPC_DISRC,PPC_GETCRFD,8,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,20,0
.51:DC.W    PPC_CODE,1,63,5,PPC_DISRC,PPC_GETCRFD,8,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,20,0
.52:DC.W    PPC_CODE,2,63,5,814,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.53:DC.W    PPC_CODE,2,63,5,815,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.54:DC.W    PPC_CODE,2,63,5,14,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.55:DC.W    PPC_CODE,2,63,5,15,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.56:DC.W    PPC_CODE,2,63,5,18,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,20,0
.57:DC.W    PPC_CODE,2,59,5,18,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,20,0
.58:DC.W    PPC_CODE,2,63,5,29,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,PPC_COMMA,PPC_ASKFPR,20,0
.59:DC.W    PPC_CODE,2,59,5,29,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,PPC_COMMA,PPC_ASKFPR,20,0
.5A:DC.W    PPC_CODE,2,63,5,72,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.5B:DC.W    PPC_CODE,2,63,5,28,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,PPC_COMMA,PPC_ASKFPR,20,0
.5C:DC.W    PPC_CODE,2,59,5,28,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,PPC_COMMA,PPC_ASKFPR,20,0
.5D:DC.W    PPC_CODE,2,63,5,25,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,0
.5E:DC.W    PPC_CODE,2,59,5,25,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,0
.5F:DC.W    PPC_CODE,2,63,5,136,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.60:DC.W    PPC_CODE,2,63,5,40,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.61:DC.W    PPC_CODE,2,63,5,31,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,PPC_COMMA,PPC_ASKFPR,20,0
.62:DC.W    PPC_CODE,2,59,5,31,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,PPC_COMMA,PPC_ASKFPR,20,0
.63:DC.W    PPC_CODE,2,63,5,30,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,PPC_COMMA,PPC_ASKFPR,20,0
.64:DC.W    PPC_CODE,2,59,5,30,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,PPC_COMMA,PPC_ASKFPR,20,0
.65:DC.W    PPC_CODE,2,59,5,24,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.66:DC.W    PPC_CODE,2,63,5,12,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.67:DC.W    PPC_CODE,2,63,5,26,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.68:DC.W    PPC_CODE,2,63,5,23,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,25,PPC_COMMA,PPC_ASKFPR,20,0
.69:DC.W    PPC_CODE,2,63,5,22,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.6A:DC.W    PPC_CODE,2,59,5,22,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,20,0
.6B:DC.W    PPC_CODE,2,63,5,20,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,20,0
.6C:DC.W    PPC_CODE,2,59,5,20,30,PPC_DORC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKFPR,15,PPC_COMMA,PPC_ASKFPR,20,0
.6D:DC.W    PPC_CODE,2,31,5,982,30,PPC_DISRC,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.6E:DC.W    PPC_CODE,2,19,5,150,30,PPC_DISRC,0
.6F:DC.W    PPC_CODE,1,34,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.70:DC.W    PPC_CODE,1,35,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.71:DC.W    PPC_CODE,2,31,5,119,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.72:DC.W    PPC_CODE,2,31,5,87,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.73:DC.W    PPC_CODE,1,58,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFC,15,0
.74:DC.W    PPC_CODE,2,31,5,84,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.75:DC.W    PPC_CODE,2,58,5,1,31,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFC,15,0
.76:DC.W    PPC_CODE,2,31,5,53,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.77:DC.W    PPC_CODE,2,31,5,21,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.78:DC.W    PPC_CODE,1,50,5,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.79:DC.W    PPC_CODE,1,51,5,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.7A:DC.W    PPC_CODE,2,31,5,631,30,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.7B:DC.W    PPC_CODE,2,31,5,599,30,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.7C:DC.W    PPC_CODE,1,48,5,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.7D:DC.W    PPC_CODE,1,49,5,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.7E:DC.W    PPC_CODE,2,31,5,567,30,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.7F:DC.W    PPC_CODE,2,31,5,535,30,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.80:DC.W    PPC_CODE,1,42,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.81:DC.W    PPC_CODE,1,43,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.82:DC.W    PPC_CODE,2,31,5,375,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.83:DC.W    PPC_CODE,2,31,5,343,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.84:DC.W    PPC_CODE,2,31,5,790,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.85:DC.W    PPC_CODE,1,40,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.86:DC.W    PPC_CODE,1,41,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.87:DC.W    PPC_CODE,2,31,5,311,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.88:DC.W    PPC_CODE,2,31,5,279,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.89:DC.W    PPC_CODE,1,46,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.8A:DC.W    PPC_CODE,2,31,5,597,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_GET5BIT,20,0
.8B:DC.W    PPC_CODE,2,31,5,533,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.8C:DC.W    PPC_CODE,2,58,5,2,31,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFC,15,0
.8D:DC.W    PPC_CODE,2,31,5,20,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.8E:DC.W    PPC_CODE,2,31,5,373,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.8F:DC.W    PPC_CODE,2,31,5,341,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.90:DC.W    PPC_CODE,2,31,5,534,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.91:DC.W    PPC_CODE,1,32,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.92:DC.W    PPC_CODE,1,33,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.93:DC.W    PPC_CODE,2,31,5,55,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.94:DC.W    PPC_CODE,2,31,5,23,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GPRZERO,15,PPC_COMMA,PPC_ASKGPR,20,0
.95:DC.W    PPC_CODE,1,19,5,PPC_DISRC,PPC_GETCRFD,8,PPC_COMMA,PPC_GETCRFD,13,0
.96:DC.W    PPC_CODE,2,63,5,64,30,PPC_DISRC,PPC_GETCRFD,8,PPC_COMMA,PPC_GETCRFD,13,0
.97:DC.W    PPC_CODE,2,31,5,512,30,PPC_DISRC,PPC_GETCRFD,8,0
.98:DC.W    PPC_CODE,2,31,5,19,30,PPC_DISRC,PPC_ASKGPR,10,0
.99:DC.W    PPC_CODE,2,63,5,583,30,PPC_DORC,PPC_ASKGPR,10,0
.9A:DC.W    PPC_CODE,2,31,5,83,30,PPC_DISRC,PPC_ASKGPR,10,0
.9B:DC.W    PPC_CODE,2,31,5,339,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKSPR,20,0
.9C:DC.W    PPC_CODE,3,31,5,339,30,1,15,PPC_DISRC,PPC_ASKGPR,10,0
.9D:DC.W    PPC_CODE,3,31,5,339,30,8,15,PPC_DISRC,PPC_ASKGPR,10,0
.9E:DC.W    PPC_CODE,3,31,5,339,30,9,15,PPC_DISRC,PPC_ASKGPR,10,0
.9F:DC.W    PPC_CODE,2,31,5,595,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_GET4BIT,15,0
.A0:DC.W    PPC_CODE,2,31,5,659,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.A1:DC.W    PPC_CODE,2,31,5,371,30,PPC_DISRC,PPC_ASKGPR,10,PPC_GETTB,20,0
.A2:DC.W    PPC_CODE,4,31,5,8,20,13,15,371,30,PPC_DISRC,PPC_ASKGPR,10,0
.A3:DC.W    PPC_CODE,2,31,5,144,30,PPC_DISRC,PPC_GETXBIT,8,19,PPC_COMMA,PPC_ASKGPR,10,0
.A4:DC.W    PPC_CODE,3,31,5,255,19,144,30,PPC_DISRC,PPC_ASKGPR,10,0
.A5:DC.W    PPC_CODE,2,63,5,70,30,PPC_DORC,PPC_GET5BIT,10,0
.A6:DC.W    PPC_CODE,2,63,5,38,30,PPC_DORC,PPC_GET5BIT,10,0
.A7:DC.W    PPC_CODE,2,63,5,711,30,PPC_DORC,PPC_GETXBIT,8,14,PPC_COMMA,PPC_ASKFPR,20,0
.A8:DC.W    PPC_CODE,2,63,5,134,30,PPC_DORC,PPC_GETCRFD,8,PPC_COMMA,PPC_GET4BIT,19,0
.A9:DC.W    PPC_CODE,2,31,5,146,30,PPC_DISRC,PPC_ASKGPR,10,0
.AA:DC.W    PPC_CODE,2,31,5,178,30,PPC_DISRC,PPC_ASKGPR,10,0
.AB:DC.W    PPC_CODE,2,31,5,467,30,PPC_DISRC,PPC_ASKSPR,20,PPC_COMMA,PPC_ASKGPR,10,0
.AC:DC.W    PPC_CODE,2,31,5,210,30,PPC_DISRC,PPC_GET4BIT,15,PPC_COMMA,PPC_ASKGPR,10,0
.AD:DC.W    PPC_CODE,2,31,5,82,30,PPC_DISRC,PPC_GET4BIT,15,PPC_COMMA,PPC_ASKGPR,10,0
.AE:DC.W    PPC_CODE,2,31,5,114,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.AF:DC.W    PPC_CODE,2,31,5,242,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.B0:DC.W    PPC_CODE,2,31,5,73,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.B1:DC.W    PPC_CODE,2,31,5,9,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.B2:DC.W    PPC_CODE,2,31,5,75,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.B3:DC.W    PPC_CODE,2,31,5,11,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.B4:DC.W    PPC_CODE,2,31,5,233,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.B5:DC.W    PPC_CODE,3,31,5,1,21,233,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.B6:DC.W    PPC_CODE,1,7,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.B7:DC.W    PPC_CODE,2,31,5,235,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,PPC_COMMA,PPC_ASKGPR,25,0
.B8:DC.W    PPC_CODE,3,31,5,1,21,235,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.B9:DC.W    PPC_CODE,2,31,5,476,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.BA:DC.W    PPC_CODE,2,31,5,104,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,0
.BB:DC.W    PPC_CODE,3,31,5,1,21,104,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,0
.BC:DC.W    PPC_CODE,2,31,5,124,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.BD:DC.W    PPC_CODE,2,31,5,444,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.BE:DC.W    PPC_CODE,2,31,5,444,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_DBLGPR,10,20,0
.BF:DC.W    PPC_CODE,2,31,5,412,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.C0:DC.W    PPC_CODE,1,24,5,PPC_DISRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_SIMM,31,0
.C1:DC.W    PPC_CODE,1,24,5,PPC_DISRC,0
.C2:DC.W    PPC_CODE,1,25,5,PPC_DISRC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_SIMM,31,0
.C3:DC.W    PPC_CODE,2,19,5,50,30,PPC_DISRC,0
.C4:DC.W    PPC_CODE,2,19,5,18,30,PPC_DISRC,0
.C5:DC.W    PPC_CODE,2,30,5,8,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,PPC_COMMA,PPC_GET5BIT,26,0
.C6:DC.W    PPC_CODE,2,30,5,8,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.C7:DC.W    PPC_CODE,2,30,5,9,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,PPC_COMMA,PPC_GET5BIT,26,0
.C8:DC.W    PPC_CODE,2,30,5,2,29,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_SPLIT6B,30,20,PPC_COMMA,PPC_GETXBIT,6,26,0
.C9:DC.W    PPC_CODE,1,30,5,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_SPLIT6B,30,20,PPC_COMMA,PPC_GETXBIT,6,26,0
.CA:DC.W    PPC_CODE,2,30,5,1,29,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_SPLIT6B,30,20,PPC_COMMA,PPC_GETXBIT,6,26,0
.CB:DC.W    PPC_CODE,2,30,5,3,29,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_SPLIT6B,30,20,PPC_COMMA,PPC_GETXBIT,6,26,0
.CC:DC.W    PPC_CODE,1,20,5,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_GET5BIT,20,PPC_COMMA,PPC_GET5BIT,25,PPC_COMMA,PPC_GET5BIT,30,0
.CD:DC.W    PPC_CODE,1,21,5,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_GET5BIT,20,PPC_COMMA,PPC_GET5BIT,25,PPC_COMMA,PPC_GET5BIT,30,0
.CE:DC.W    PPC_CODE,1,23,5,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,PPC_COMMA,PPC_GET5BIT,25,PPC_COMMA,PPC_GET5BIT,30,0
.CF:DC.W    PPC_CODE,2,17,5,1,30,PPC_DISRC,0
.D0:DC.W    PPC_CODE,2,31,5,498,30,PPC_DISRC,0
.D1:DC.W    PPC_CODE,2,31,5,434,30,PPC_DISRC,PPC_ASKGPR,20,0
.D2:DC.W    PPC_CODE,2,31,5,27,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.D3:DC.W    PPC_CODE,2,31,5,24,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.D4:DC.W    PPC_CODE,2,31,5,794,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.D5:DC.W    PPC_CODE,2,31,5,413,29,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_SPLIT6B,30,20,0
.D6:DC.W    PPC_CODE,2,31,5,792,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.D7:DC.W    PPC_CODE,2,31,5,824,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_GET5BIT,20,0
.D8:DC.W    PPC_CODE,2,31,5,539,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.D9:DC.W    PPC_CODE,2,31,5,536,30,PPC_DORC,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,20,0
.DA:DC.W    PPC_CODE,1,38,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.DB:DC.W    PPC_CODE,1,39,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.DC:DC.W    PPC_CODE,2,31,5,247,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.DD:DC.W    PPC_CODE,2,31,5,215,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.DE:DC.W    PPC_CODE,1,62,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFC,15,0
.DF:DC.W    PPC_CODE,2,31,5,214,30,PPC_REQRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.E0:DC.W    PPC_CODE,2,62,5,1,31,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFC,15,0
.E1:DC.W    PPC_CODE,2,31,5,181,30,PPC_REQRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.E2:DC.W    PPC_CODE,2,31,5,149,30,PPC_REQRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.E3:DC.W    PPC_CODE,1,54,5,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.E4:DC.W    PPC_CODE,1,55,5,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.E5:DC.W    PPC_CODE,2,31,5,759,30,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.E6:DC.W    PPC_CODE,2,31,5,727,30,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.E7:DC.W    PPC_CODE,2,31,5,983,30,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.E8:DC.W    PPC_CODE,1,52,5,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.E9:DC.W    PPC_CODE,1,53,5,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.EA:DC.W    PPC_CODE,2,31,5,695,30,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.EB:DC.W    PPC_CODE,2,31,5,663,30,PPC_DISRC,PPC_ASKFPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.EC:DC.W    PPC_CODE,1,44,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.ED:DC.W    PPC_CODE,2,31,5,918,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.EE:DC.W    PPC_CODE,1,45,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.EF:DC.W    PPC_CODE,2,31,5,439,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.F0:DC.W    PPC_CODE,2,31,5,407,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.F1:DC.W    PPC_CODE,1,47,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.F2:DC.W    PPC_CODE,2,31,5,729,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_GET5BIT,20,0
.F3:DC.W    PPC_CODE,2,31,5,661,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.F4:DC.W    PPC_CODE,1,36,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.F5:DC.W    PPC_CODE,2,31,5,662,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.F6:DC.W    PPC_CODE,3,31,5,150,30,1,31,PPC_REQRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.F7:DC.W    PPC_CODE,1,37,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_RELATIVE,31,$FFFF,15,0
.F8:DC.W    PPC_CODE,2,31,5,183,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.F9:DC.W    PPC_CODE,2,31,5,151,30,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.FA:DC.W    PPC_CODE,2,31,5,40,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.FB:DC.W    PPC_CODE,3,31,5,1,21,40,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.FC:DC.W    PPC_CODE,2,31,5,8,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.FD:DC.W    PPC_CODE,3,31,5,1,21,8,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.FE:DC.W    PPC_CODE,2,31,5,136,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.FF:DC.W    PPC_CODE,3,31,5,1,21,136,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.G0:DC.W    PPC_CODE,1,8,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.G1:DC.W    PPC_CODE,2,31,5,232,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,0
.G2:DC.W    PPC_CODE,3,31,5,1,21,232,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,0
.G3:DC.W    PPC_CODE,2,31,5,200,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,0
.G4:DC.W    PPC_CODE,3,31,5,1,21,200,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,0
.G5:DC.W    PPC_CODE,2,31,5,598,30,PPC_DISRC,0
.G6:DC.W    PPC_CODE,2,31,5,68,30,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.G7:DC.W    PPC_CODE,1,2,5,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.G8:DC.W    PPC_CODE,2,31,5,370,30,PPC_DISRC,0
.G9:DC.W    PPC_CODE,2,31,5,306,30,PPC_DISRC,PPC_ASKGPR,20,0
.GA:DC.W    PPC_CODE,2,31,5,566,30,PPC_DISRC,0
.GB:DC.W    PPC_CODE,2,31,5,68,4,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.GC:DC.W    PPC_CODE,1,3,5,PPC_DISRC,PPC_GET5BIT,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.GD:DC.W    PPC_CODE,2,31,5,316,30,PPC_DORC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_ASKGPR,20,0
.GE:DC.W    PPC_CODE,1,26,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0
.GF:DC.W    PPC_CODE,1,27,5,PPC_DISRC,PPC_ASKGPR,10,PPC_COMMA,PPC_ASKGPR,15,PPC_COMMA,PPC_SIMM,31,0

;*-*

;; Instable
INSTABLE:             ; +IOFF
    DC.L    .0,00,.1,00,.2,00,.3,00,.4,00,.5,00,.6,00,.7,00,.8,00,.9,00,.10,00,.11,00
    DC.L    .12,0,.13,0,.14,0,.15,0,.16,0,.17,0,.18,0,.19,0,.20,0,.21,0
    DC.L    .22,0,.23,0,.24,0,.25,0,.26,0,.27,0,.28,0,.29,0,.30,0,.31,0
    DC.L    .32,0,.33,0,.34,0,.35,0,.36,0,.37,0,.38,0,.39,0,.40,0,.41,0
    DC.L    .42,0,.43,0,.44,0,.45,0,.46,0,.47,0,.48,0,.49,0,.50,0,.51,0
    DC.L    .52,0,.53,0,.54,0,.55,0,.56,0,.57,0,.58,0,.59,0,.60,0,.61,0
    DC.L    .62,0,.63,0,.64,0,.65,0,.66,0,.67,0,.68,0,.69,0,.70,0,.71,0
    DC.L    .72,0,.73,0,.74,0,.75,0,.76,0,.77,0,.78,0,.79,0,.80,0,.81,0
    DC.L    .82,0,.83,0,.84,0,.85,0,.86,0,.87,0,.88,0,.89,0,.90,0,.91,0
    DC.L    .92,0,.93,0

    DC.L    0,0
.0:     DC.B    'PROC',0
.1:     DC.B    'ENDPROC',0
.2:     DC.B    'IF',0
.3:     DC.B    'ENDIF',0
.4:     DC.B    'VOID',0
.5:     DC.B    'WHILE',0
.6:     DC.B    'ENDWHILE',0
.7:     DC.B    'FOR',0
.8:     DC.B    'ENDFOR',0
.9:     DC.B    'SELECT',0
.10:    DC.B    'CASE',0
.11:    DC.B    'DEFAULT',0
.12:    DC.B    'ENDSELECT',0
.13:    DC.B    'REPEAT',0
.14:    DC.B    'UNTIL',0
.15:    DC.B    'JUMP',0
.16:    DC.B    'DEF',0
.17:    DC.B    0           ; IS NOG _WEL_ LOCAL !!!
.18:    DC.B    'ELSE',0
.19:    DC.B    'INCBIN',0
.20:    DC.B    'LONG',0
.21:    DC.B    'INT',0
.22:    DC.B    'CHAR',0
.23:    DC.B    'INC',0
.24:    DC.B    'DEC',0
.25:    DC.B    'THEN',0
.26:    DC.B    'LOOP',0
.27:    DC.B    'ENDLOOP',0
.28:    DC.B    'DO',0
.29:    DC.B    'AND',0
.30:    DC.B    'OR',0
.31:    DC.B    'CONST',0
.32:    DC.B    'OPT',0
.33:    DC.B    'MODULE',0
.34:    DC.B    'STACK',0
.35:    DC.B    'EXIT',0
.36:    DC.B    'LARGE',0
.37:    DC.B    'ASM',0
.38:    DC.B    'NOWARN',0
.39:    DC.B    'TO',0
.40:    DC.B    'STEP',0
.41:    DC.B    'ARRAY',0
.42:    DC.B    'STRING',0
.43:    DC.B    'DIR',0
.44:    DC.B    'PTR',0
.45:    DC.B    'OF',0
.46:    DC.B    'ELSEIF',0
.47:    DC.B    'LIST',0
.48:    DC.B    'OBJECT',0
.49:    DC.B    'ENDOBJECT',0
.50:    DC.B    'SIZEOF',0
.51:    DC.B    'RETURN',0
.52:    DC.B    'OSVERSION',0
.53:    DC.B    'ENUM',0
.54:    DC.B    'SET',0
.55:    DC.B    'BUT',0
.56:    DC.B    'HANDLE',0
.57:    DC.B    'EXCEPT',0
.58:    DC.B    'RAISE',0
.59:    DC.B    'EXPORT',0
.60:    DC.B    'REG',0
.61:    DC.B    'END',0
.62:    DC.B    'IS',0
.63:    DC.B    'NEW',0
.64:    DC.B    'PUBLIC',0
.65:    DC.B    'PRIVATE',0
.66:    DC.B    'SUPER',0
.67:    DC.B    'PREPROCESS',0
.68:    DC.B    'LIBRARY',0
.69:    DC.B    'UTILLIB',0
.70:    DC.B    'POOL',0
.71:    DC.B    'NOSTARTUP',0
.72:    DC.B    'LINKABLE',0
.73:    DC.B    'EXTRA',0
.74:    DC.B    'INLINE',0
.75:    DC.B    'UNION',0
.76:    DC.B    'INCLIB',0
.77:    DC.B    'STARTUP',0
.78:    DC.B    'WITH',0
.79:    DC.B    'ELSEWHILE',0
.80:    DC.B    'ALWAYS',0
.81:    DC.B    'FPEXP',0
.82:    DC.B    'FLOAT',0
.83:    DC.B    'SECTION',0
.84:    DC.B    'CODE',0
.85:    DC.B    'DATA',0
.86:    DC.B    'CHIP',0
.87:    DC.B    'FAST',0
.88:    DC.B    'PPC',0
.89:    DC.B    'RUNBG',0
.90:    DC.B    'SETUP',0
.91:    DC.B    'CLEANUP',0
.92:    DC.B    'SMALL',0
.93:    DC.B    'NOREGS',0
    EVEN
;*-*
;;Jobtab

; ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ;
;   Instruction JobCode And Data Part           ;
; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,, ;

DUM:
INSJOBTAB:
    DC.L    .PROC,.ENDPROC,.IF,.ENDIF,.VOID,.WHILE,.ENDWHILE        ; 0-6
    DC.L    .FOR,.ENDFOR,.SELECT,.CASE,.DEFAULT,.ENDSELECT          ; 7-12
    DC.L    .REPEAT,.UNTIL,.JUMP,.DEF,.LOCAL,.ELSE,.INCBIN          ; 13-19
    DC.L    .LONG,.INT,.CHAR,.INC,.DEC,.THEN,.LOOP,.ENDLOOP         ; 20-27
    DC.L    .ERROR,.ERROR,.ERROR,.ERROR,.ERROR,.ERROR,.ERROR        ; 28-34
    DC.L    .EXIT,.ERROR,.ERROR,.ERROR,.ERROR,.ERROR,.ERROR         ; 35-41
    DC.L    .ERROR,.ERROR,.ERROR,.ERROR,.ELSEIF,.ERROR,.OBJECT      ; 42-48
    DC.L    .ERROR,.ERROR,.RETURN,.ERROR,.ERROR,.ERROR,.ERROR       ; 49-55
    DC.L    .ERROR,.EXCEPT,.ERROR,.ERROR,.ERROR,.END,.ERROR         ; 56-62
    DC.L    .NEW,.ERROR,.ERROR,.SUPER,.ERROR,.ERROR,.ERROR,.ERROR   ; 63-70
    DC.L    .ERROR,.ERROR,.ERROR,.LOOP,.ERROR,.ERROR,.ERROR,.ERROR  ; 71-78
    DC.L    .ELSEWHILE,.ALWAYS,.ERROR,.ERROR,.SECTION               ; 79-83

.SECTION:
        DC.W    79,0

.PROC:  DC.W    50,81,19,17,36,33,1,37,10
        LINK    A5,#-27*4
        DC.W    34,73,72,80,63,5,1,70,7,1,65,16,18,9
        UNLK    A5
        DC.W    77,68,0

.ENDPROC:
        DC.W    7,1,65,16,81,18,9
        UNLK    A5
        DC.W    77,68,0

.RETURN:
        DC.W    16,58,0

.EXIT:  DC.W    61,56
        TST.L   D0
        BEQ DUM
        DC.W    75,0

.IF:    DC.W    51,6,61,56
        TST.L   D0
        BEQ DUM
        DC.W    8,5,2,43,IOFF+25,60,45,IOFF+18,7,2,28,10
        BRA DUM
        DC.W    57,29,27,15,26,5,2,60,46,0,7,2,15,54,0

.ELSE:  DC.W    7,2,28,10
        BRA DUM
        DC.W    29,27,15,26,5,2,0

.ELSEIF:DC.W    7,2,28,10
        BRA DUM
        DC.W    27,15,52,26,2,12
        TST.L   D0
        BEQ DUM
        DC.W    29,5,2,0

.ENDIF: DC.W    7,2,15,53,0

.VOID:  DC.W    2,0

.WHILE: DC.W    51,6,20,61,56
        TST.L   D0
        BEQ DUM
        DC.W    8,5,3

        DC.W 43,IOFF+28,42,7,3,21,10
        BRA DUM
        DC.W    8,15,53,0

.ELSEWHILE:
        DC.W    7,3,27,10
        BRA DUM
        DC.W    15,52,21,8,25,20,26,61,56
        TST.L   D0
        BEQ DUM
        DC.W    27,21,8,25,26,5,3,0
.ALWAYS:
        DC.W    7,3,27
        DC.W    21,53,20,25,26
        DC.W    5,3,0

.ENDWHILE:
        DC.W    7,3,21,27,53,26,10
        BRA DUM
        DC.W    8,15,0

.FOR:   DC.W    3,47,2,10
        MOVE.L  D0,2(A5)
        DC.W    35

        DC.W 49,5,4,43,IOFF+28,42,7,4,62
        DC.W    15;,21

        DC.W    78

        DC.W 0

.ENDFOR:DC.W    7,4,62
        DC.W    15;,21

        DC.W    78

        DC.W    0

.SELECT:DC.W    71,2,9
        MOVE.L  D0,-(A7)
        DC.W    6,28,32,5,5,0

.CASE:  DC.W    7,5,27,31,3,10
        BRA DUM
        DC.W    29,30,15,2,12
        CMP.L   (A7),D0
        BNE DUM
        DC.W    6,8
        DC.w    9
        ADDQ.L  #4,a7
        DC.W    26,5,5,0

.DEFAULT:
        DC.W    7,5,27,10
        BRA DUM
        DC.W    29,15,6,26
        DC.W    9
        ADDQ.L  #4,A7
        DC.W    5,5,0

.ENDSELECT:
        DC.W    7,5
        DC.W    9
        BRA.B   *+4
        DC.W    21,15,9
        ADDQ.L  #4,A7
        DC.W    25
        DC.W    15
        DC.W    0

.REPEAT:DC.W    20,5,6,0

.UNTIL: DC.W    7,6,21,61,56
        TST.L   D0
        BEQ DUM
        DC.W    8,0

.JUMP:  DC.W    1,10
        BRA DUM
        DC.W    59,0

.DEF:   DC.W    33,3,0

.LOCAL: DC.W    33,2,0

.INCBIN:DC.W    38,0

.LONG:  DC.W    39,0

.INT:   DC.W    40,0

.CHAR:  DC.W    41,0

.DEC:   DC.W    3,10
        SUBQ.L  #1,-4(A5)
        DC.W    22,0

.INC:   DC.W    3,10
        ADDQ.L  #1,-4(A5)
        DC.W    22,0

.THEN:  DC.W    44,0

.LOOP:  DC.W    20,5,7,0

.ENDLOOP:
        DC.W    7,7,21,10
        BRA DUM
        DC.W    8,0

.ERROR: DC.W    44,0

.OBJECT:DC.W    55,0

.EXCEPT:DC.W    7,1,64,5,1,0

.NEW:   DC.W    69,0

.END:   DC.W    74,0

.SUPER: DC.W    76,0

;*-*

;; Patcher
PCH_OS      =       0
PCH_CPU     =       1
PCH_FPU     =       2
PCH_UTIL    =       3
PCH_POOL    =       4
PCH_INL     =       5       ; = NONE!!!
INL         =       $80000000
; structure:
;       cmd_nr:LONG, pch_type:LONG, pch_param:LONG, start:LONG, end:LONG


PATCHES:
        DC.L    1,      PCH_OS,         37,         I_PRINTF37,         I_PRINTF37_E
        DC.L    87,     PCH_OS,         37,         I_PRINTF37,         I_PRINTF37_E
        DC.L    150,    PCH_OS,         37,         I_PUTF37,           I_PUTF37_E

        DC.L    13,     INL+PCH_INL,    0,          0,                  0
        DC.L    14,     INL+PCH_INL,    0,          0,                  0
        DC.L    15,     INL+PCH_INL,    0,          0,                  0
        DC.L    16,     INL+PCH_INL,    0,          0,                  0
        DC.L    17,     INL+PCH_INL,    0,          0,                  0
        DC.L    18,     INL+PCH_INL,    0,          0,                  0
        DC.L    20,     INL+PCH_INL,    0,          0,                  0
        DC.L    23,     INL+PCH_INL,    0,          0,                  0
        DC.L    24,     INL+PCH_INL,    0,          0,                  0
        DC.L    25,     INL+PCH_INL,    0,          0,                  0
        DC.L    48,     INL+PCH_INL,    0,          0,                  0
        DC.L    49,     INL+PCH_INL,    0,          0,                  0
        DC.L    58,     INL+PCH_INL,    0,          0,                  0
        DC.L    59,     INL+PCH_INL,    0,          0,                  0
        DC.L    60,     INL+PCH_INL,    0,          0,                  0
        DC.L    64,     INL+PCH_INL,    0,          0,                  0
        DC.L    65,     INL+PCH_INL,    0,          0,                  0
        DC.L    66,     INL+PCH_INL,    0,          0,                  0
        DC.L    76,     INL+PCH_INL,    0,          0,                  0
        DC.L    77,     INL+PCH_INL,    0,          0,                  0
        DC.L    78,     INL+PCH_INL,    0,          0,                  0
        DC.L    80,     INL+PCH_INL,    0,          0,                  0
        DC.L    81,     INL+PCH_INL,    0,          0,                  0
        DC.L    82,     INL+PCH_INL,    0,          0,                  0
        DC.L    99,     INL+PCH_INL,    0,          0,                  0
        DC.L    100,    INL+PCH_INL,    0,          0,                  0
        DC.L    101,    INL+PCH_INL,    0,          0,                  0
        DC.L    102,    INL+PCH_INL,    0,          0,                  0
        DC.L    103,    INL+PCH_INL,    0,          0,                  0
        DC.L    104,    INL+PCH_INL,    0,          0,                  0
        DC.L    105,    INL+PCH_INL,    0,          0,                  0
        DC.L    106,    INL+PCH_INL,    0,          0,                  0
        DC.L    107,    INL+PCH_INL,    0,          0,                  0
        DC.L    108,    INL+PCH_INL,    0,          0,                  0
        DC.L    109,    INL+PCH_INL,    0,          0,                  0
        DC.L    118,    INL+PCH_INL,    0,          0,                  0
        DC.L    119,    INL+PCH_INL,    0,          0,                  0
        DC.L    123,    INL+PCH_INL,    0,          0,                  0
        DC.L    124,    INL+PCH_INL,    0,          0,                  0
        DC.L    125,    INL+PCH_INL,    0,          0,                  0
        DC.L    126,    INL+PCH_INL,    0,          0,                  0
        DC.L    127,    INL+PCH_INL,    0,          0,                  0
        DC.L    128,    INL+PCH_INL,    0,          0,                  0
        DC.L    129,    INL+PCH_INL,    0,          0,                  0
        DC.L    130,    INL+PCH_INL,    0,          0,                  0
        DC.L    131,    INL+PCH_INL,    0,          0,                  0
        DC.L    156,    INL+PCH_INL,    0,          0,                  0
        DC.L    157,    INL+PCH_INL,    0,          0,                  0

        DC.L    2,      INL+PCH_CPU,    %01110,     0,                  0
        DC.L    3,      INL+PCH_CPU,    %01110,     0,                  0
        DC.L    81,     INL+PCH_CPU,    %01110,     0,                  0

        DC.L    13,     PCH_CPU,        %11110,     I_LONG020,          I_INT020
        DC.L    14,     PCH_CPU,        %11110,     I_INT020,           I_CHAR020
        DC.L    15,     PCH_CPU,        %11110,     I_CHAR020,          I_PATCHEND



        DC.L    -1,     0,              0,          0,                  0
;*-*
;*-*

