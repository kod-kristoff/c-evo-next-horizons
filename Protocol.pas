{$INCLUDE Switches.inc}
{$HINTS OFF}
unit Protocol;

interface

const
  lxmax = 100;
  lymax = 96;
  nAdv = 94; { number of advances }
  nImp = 70; { number of improvements }
  nPl = 15; { max number of players, don't change! }
  nWonder = 28; { number of wonders }
  nUmax = 4096; { max units/player, don't set above 4096 }
  nCmax = 1024; { max cities/player, don't set above 4096 }
  nMmax = 256; { max models/player, don't set above 1024 }
  nExp = 5; // number of experience levels
  ExpCost = 50; { received damage required for next experience level }
  MaxFutureTech = 25;
  // maximum number of future techs of one kind except computing technology
  MaxFutureTech_Computing = 100;
  // maximum number of computing technology future techs
  CountryRadius = 9;
  MaxCitySize = 30;
  BasicHappy = 2; { basically happy citizens }
  MaxPollution = 240;
  NeedAqueductSize = 8;
  NeedSewerSize = 12;
  ColossusEffect = 75; // percent wonder building cost
  UniversityFutureBonus = 5; // percent per tech
  ResLabFutureBonus = 10; // percent per tech
  FactoryFutureBonus = 5; // percent per tech
  MfgPlantFutureBonus = 10; // percent per tech
  AnarchyTurns = 3;
  CaptureTurns = 3;
  CancelTreatyTurns = 3;
  PeaceEvaTurns = 5;
  // should be less then 2*CancelTreatyTurns, so that you can't attack an ally without re-entering
  ColdWarTurns = 40;
  DesertThurst = 20; // damage for turn in desert
  ArcticThurst = 20; // damage for turn in arctic
  FastRecovery = 50;
  CityRecovery = 20;
  NoCityRecovery = 8;
  MaxMoneyPrice = $FFFF;
  MaxShipPartPrice = 100;
  BombardmentDestroysCity = False;
  StartMoney = 0;
  InitialCredibility = 95;

  // ai module flags (for TInitModuleData.Flags)
  aiThreaded = $01;

  // difficulty settings
  MaxDiff = 4; { maximum difficulty level }
  StorageSize: array [1 .. MaxDiff] of Integer = (30, 40, 50, 60);
  TechFormula_M: array [1 .. MaxDiff] of Single = (2.0, 2.3, 2.6, 4.0);
  TechFormula_D: array [1 .. MaxDiff] of Single = (102.0, 80.0, 64.0, 64.0);
  BuildCostMod: array [1 .. MaxDiff] of Integer = (9, 12, 15, 18); // in 1/12

  // test flags
  nTestFlags = 7; // max. 11
  tfAllTechs = $001; { all nations get all techs }
  tfImmImprove = $002; { city projects complete each turn }
  tfImmAdvance = $004; { research complete each turn }
  tfImmGrow = $008; { all cities grow in each turn }
  tfUncover = $010; // all players see like supervisor
  tfAllContact = $020; // all nations can contact each other
  tfNoRareNeed = $040; // producing colony ship requires no modern resources
  tfTested = $800; // at least one test flag was set

  { server commands
    IMPORTANT: lowest 4 bits must indicate size in DWORDS of data parameter,
    except for request commands }

  sctMask = $3800; // server command type
  sExecute = $4000; { call command-sExecute to request return value without
    execution }
  cClientEx = $8000;

  // Info Request Commands
  sctInfo = $0000;
  sMessage = $0000;
  sSetDebugMap = $0010;
  sGetDebugMap = $0020;
  { sChangeSuperView=$0030; } sRefreshDebugMap = $0040;
  sGetChart = $0100; // + type shl 4
  sGetTechCost = $0180;
  sGetAIInfo = $01C0;
  sGetAICredits = $01D0;
  sGetVersion = $01E0;
  sGetGameChanged = $01F0;
  sGetTileInfo = $0200;
  sGetCityTileInfo = $0210;
  sGetHypoCityTileInfo = $0220;
  sGetJobProgress = $0230;
  sGetModels = $0270;
  sGetUnits = $0280;
  sGetDefender = $0290;
  sGetBattleForecast = $02A0;
  sGetUnitReport = $02B0;
  sGetMoveAdvice = $02C0;
  sGetPlaneReturn = $02D0;
  sGetBattleForecastEx = $02E0;
  sGetCity = $0300;
  sGetCityReport = $0310;
  sGetCityAreaInfo = $0320;
  sGetEnemyCityReport = $0330;
  sGetEnemyCityAreaInfo = $0340;
  sGetCityTileAdvice = $0350;
  sGetCityReportNew = $0360;
  sGetEnemyCityReportNew = $0370;

  // Map Editor Commands
  sEditTile = $0710;
  sRandomMap = $0780;
  sMapGeneratorRequest = $0790;

  // Server Internal Commands
  sctInternal = sctInfo;
  // sctInfo - without sExecute flag, sctInternal - with sExecute flag
  sIntTellAboutNation = $4000;
  sIntHaveContact = $4010;
  sIntCancelTreaty = $4020;
  sIntTellAboutModel = $4100; { +told player shl 4 }
  sIntDiscoverZOC = $4201;
  sIntExpandTerritory = $4218;
  sIntBuyMaterial = $4301;
  sIntPayPrices = $4402;
  sIntSetDevModel = $450D;
  sIntSetModelStatus = $4601;
  sIntSetUnitStatus = $4611;
  sIntSetCityStatus = $4621;
  sIntSetECityStatus = $4631;
  sIntDataChange = $4700;

  // Client Deactivation Commands
  sctEndClient = $0800;
  sTurn = $4800;
  sBreak = $4810;
  sResign = $4820;
  sNextRound = $4830;
  sReload = $4841;
  sSaveMap = $4880;
  sAbandonMap = $4890;
  // diplomacy commands equal to client, see below

  // General Commands
  sctGeneral = $1000;
  sClearTestFlag = $5000;
  sSetTestFlag = $5010;
  sSetGovernment = $5100;
  sSetRates = $5110;
  sRevolution = $5120;
  sSetResearch = $5200;
  sStealTech = $5210;
  sSetAttitude = $5300; // + concerned player shl 4
  sCancelTreaty = $5400;

  // Model Related Commands
  sctModel = $1800;
  sCreateDevModel = $5800;
  sSetDevModelCap = $5C00; { +value shl 4 }
  { reserves $5CXX, $5DXX, $5EXX, $5FXX }

  // Unit Related Commands
  sctUnit = $2000;
  sRemoveUnit = $6000;
  sSetUnitHome = $6010;
  sSetSpyMission = $6100; // + mission shl 4
  sLoadUnit = $6200;
  sUnloadUnit = $6210;
  sSelectTransport = $6220;
  sCreateUnit = $6301; // + player shl 4
  sMoveUnit = $6400; { +dx and 7 shl 4 +dy and 7 shl 7 }
  { reserves $64XX, $65XX, $66XX, $67XX }

  // Settlers Related Commands
  sctSettlers = $2800;
  sAddToCity = $6810;
  sStartJob = $6C00; { +job shl 4 }
  { reserves $6CXX, $6DXX, $6EXX, $6FXX }

  // City Related Commands
  sctCity = $3000;
  sSetCityProject = $7001;
  sBuyCityProject = $7010;
  sSellCityProject = $7020;
  sSellCityImprovement = $7101;
  sRebuildCityImprovement = $7111;
  sSetCityTiles = $7201;

  // free command space
  sctUnused = $3800;

  { client commands }
  cInitModule = $0000;
  cReleaseModule = $0100;
  cBroadcast = $0200;
  cHelpOnly = $0700;
  cStartHelp = $0710;
  cStartCredits = $0720;

  cNewGame = $0800;
  cLoadGame = $0810;
  cMovie = $0820;
  cNewGameEx = $0840;
  cLoadGameEx = $0850;
  cNewMap = $0880;
  cReplay = $08E0;
  cGetReady = $08F0;
  cBreakGame = $0900;

  cTurn = $2000;
  cResume = $2010;
  cContinue = $2080;
  cMovieTurn = $2100;
  cMovieEndTurn = $2110;
  cEditMap = $2800;

  // cShowTileM=$3000;cShowTileA=$3010;cShowFoundCity=$3020;
  cShowUnitChanged = $3030;
  cShowAfterMove = $3040;
  cShowAfterAttack = $3050;
  cShowCityChanged = $3090;
  // cShowMove=$3100;cShowCapture=$3110;
  // cShowAttackBegin=$3200;cShowAttackWon=$3210;cShowAttackLost=$3220;
  cShowMoving = $3140;
  cShowCapturing = $3150;
  cShowAttacking = $3240;
  cShowMissionResult = $3300;
  cShowShipChange = $3400;
  cShowGreatLibTech = $3500;
  cShowTurnChange = $3700;
  cShowCancelTreaty = $3800;
  cShowEndContact = $3810;
  cShowCancelTreatyByAlliance = $3820;
  cShowSupportAllianceAgainst = $3830;
  cShowPeaceViolation = $3880;
  cShowGame = $3F00; { cShowSuperView=$3F80; }
  cRefreshDebugMap = $3F90;

  // diplomacy commands equal to server, see below

  cDebugMessage = $7000;
  cShowNego = $7010;

  // commands same for server and client
  scContact = $4900; // + concerned player shl 4 for server call
  scReject = $4A00;
  scDipStart = $4B00;
  scDipNotice = $4B10;
  scDipAccept = $4B20;
  scDipCancelTreaty = $4B30;
  scDipOffer = $4B4E;
  scDipBreak = $4BF0;

  { server return codes: flags }
  rExecuted = $40000000;
  rEffective = $20000000;
  rUnitRemoved = $10000000;
  rEnemySpotted = $08000000;

  { server return codes: command executed }
  // note: the same return code might have a different meaning for different server functions!
  eOK = $60000000; // ok
  eEnemySpotted = $68000000; // unit move ok, new enemy unit/city spotted
  eDied = $70000000; // move executed, unit died due to hostile terrain
  eEnemySpotted_Died = $78000000;
  // unit move ok, new enemy unit/city spotted, unit died due to hostile terrain
  eLoaded = $60000002; // unit move caused loading to transport ship
  eLost = $70000004; // attack executed, battle lost, unit is dead
  eWon = $60000005; // attack executed, battle won, defender destroyed
  eBloody = $70000005; // attack executed, defender destroyed, unit is dead
  eBombarded = $60000006; // empty enemy city bombarded
  eExpelled = $60000007; // friendly unit expelled
  eMissionDone = $70000008;
  // spy moved into city: mission done, spy no longer exists
  eJobDone = $60000001; // settler job started and already done
  eJobDone_Died = $70000001;
  // settler job started and already done, unit died due to hostile terrain
  eCity = $70000002; // city founded, settler no more exists
  eRemoved = $70000000; // sRemoveUnit: unit removed
  eUtilized = $70000001; // sRemoveUnit: unit utilized for city project

  eNotChanged = $40000000;
  // ok, but no effect (e.g. current city project set again)

  { server return codes: command not executed }
  eHiddenUnit = $20000013;
  // unit move: not possible, destination tile occupied by hidden foreign submarine
  eStealthUnit = $2000001A;
  // unit move: not possible, destination tile occupied by foreign stealth unit
  eZOC_EnemySpotted = $28000014;
  // unit move: not possible, new enemy unit spotted, ZOC violation

  eInvalid = $0000; // command not allowed now or parameter out of allowed range
  eUnknown = $0001; // unknown command
  eNoTurn = $0002; // command only allowed during player's turn
  eViolation = $0003; // general violation of game rules
  eNoPreq = $0004; // the prerequisites for this command are not fully met

  eNoTime_Move = $0008; // normal unit move: too few movement points left
  eNoTime_Load = $0009; // load unit: too few movement points left
  eNoTime_Attack = $000A; // attack: no movement points left
  eNoTime_Bombard = $000B; // bombard city: too few movement points left
  eNoTime_Expel = $000C; // expel spy: too few movement points left

  eDomainMismatch = $0011;
  // move/attack: action not allowed for this unit domain
  eNoCapturer = $0012;
  // unit move: this type of unit is not allowed to capture a city
  eZOC = $0014; // unit move: not possible, ZOC violation
  eTreaty = $0015; // move/attack: not possible, peace treaty violation
  eDeadLands = $0016; // sStartJob: not possible, dead lands
  eNoRoad = $0017; // unit move: not possible, no road
  eNoNav = $0019; // unit move: not possible, open sea without navigation
  eNoLoadCapacity = $001B; // load to transport: no more transport capacity
  eNoBombarder = $001C; // bombardment impossible because no attack power

  eMaxSize = $0020;
  // add to city: bigger size not allowed due to missing aqueduct/sewer
  eNoCityTerrain = $0022; // found city: not possible in this terrain
  eNoBridgeBuilding = $0023;
  eInvalidOffer = $0030;
  eOfferNotAcceptable = $0031;
  eCancelTreatyRush = $0032;
  eAnarchy = $0038; // no negotiation in anarchy
  eColdWar = $003F;
  eNoModel = $0040; // sCreateDevModel must be called before!
  eTileNotAvailable = $0050;
  eNoWorkerAvailable = $0051;
  eOnlyOnce = $0058;
  // sell/rebuild city improvement: only once per city and turn!
  eObsolete = $0059; // city project: more advanced improvement already exists
  eOutOfControl = $005A;
  // buy/sell/rebuild improvement: not in anarchy, not in captured cities

  eNoWay = $0100; // sGetMoveAdvice: no way found

  // chart types
  nStat = 6;
  stPop = 0;
  stTerritory = 1;
  stMil = 2;
  stScience = 3;
  stExplore = 4;
  stWork = 5;

  { tile flags: terrain type }
  fTerrain = $1F; // mask for terrain type
  fOcean = $00;
  fShore = $01;
  fGrass = $02;
  fDesert = $03;
  fPrairie = $04;
  fTundra = $05;
  fArctic = $06;
  fSwamp = $07;
  fForest = $09;
  fHills = $0A;
  fMountains = $0B;
  fUNKNOWN = fTerrain;

  { tile flags: terrain improvements }
  fTerImp = $0000F000; // mask for terrain improvement
  tiNone = $00000000;
  tiIrrigation = $00001000;
  tiFarm = $00002000;
  tiMine = $00003000;
  tiFort = $00004000;
  tiBase = $00005000;

  { tile flags: add ons }
  fSpecial = $00000060;
  fSpecial1 = $00000020;
  fSpecial2 = $00000040;
  fRiver = $00000080;
  fRoad = $00000100;
  fRR = $00000200; // railroad
  fCanal = $00000400;
  fPoll = $00000800;
  fPrefStartPos = $00200000;
  fStartPos = $00400000; // map editor only
  fDeadLands = $01000000;
  fModern = $06000000;
  fCobalt = $02000000;
  fUranium = $04000000;
  fMercury = $06000000;

  { tile flags: redundant helper info }
  fGrWall = $00010000; // tile protected by great wall
  fSpiedOut = $00020000;
  fStealthUnit = $00040000;
  fHiddenUnit = $00080000;
  fObserved = $00100000; // set if tile information is from this turn
  fOwned = $00200000; // set if unit/city here is own one
  fUnit = $00400000;
  fCity = $00800000;
  fOwnZoCUnit = $10000000; // own ZoC unit present at this tile
  fInEnemyZoC = $20000000;
  // tile is adjacent to known foreign ZoC unit (not allied)
  fPeace = $40000000;
  // tile belongs to territory of nation that we are in peace with but not allied

  // city project flags
  cpIndex = $1FF;
  cpConscripts = $200; // produce unit as conscripts
  cpDisbandCity = $400;
  // allow to disband city when settlers/conscripts are produced
  cpImp = $800; // 0: index refers to model, 1: index refers to city improvement
  cpRepeat = $1000;
  cpCompleted = $2000;
  cpAuto = $F000; // for internal use only

  // tech status indicators
  tsNA = -2;
  tsSeen = -1;
  tsResearched = 0;
  tsGrLibrary = 1;
  tsCheat = 15;
  tsApplicable = tsResearched;

  // nation treaties
  trNoContact = -1;
  trNone = 0;
  trPeace = 2;
  trFriendlyContact = 3;
  trAlliance = 4;

  // attitudes
  nAttitude = 7;
  atHostile = 0;
  atIcy = 1;
  atUncoop = 2;
  atNeutral = 3;
  atReceptive = 4;
  atCordial = 5;
  atEnth = 6;

  // offer prices
  opChoose = $00000000;
  opCivilReport = $11000000; // + turn + concerned player shl 16
  opMilReport = $12000000; // + turn + concerned player shl 16
  opMap = $1F000000;
  opTreaty = $20000000; // + suggested nation treaty
  opShipParts = $30000000; // + number + part type shl 16
  opMoney = $40000000; // + value
  opTribute = $48000000; // obsolete
  opTech = $50000000; // + advance
  opAllTech = $51000000;
  opModel = $58000000; // + model index
  opAllModel = $59000000;

  opMask = $FF000000;

  // improvement kinds
  ikTrGoods = 0;
  ikCommon = 1;
  ikNatLocal = 2;
  ikNatGlobal = 3;
  ikWonder = 4;
  ikShipPart = 5;
  ikNA = $7F;

  { model domains }
  nDomains = 3;
  dGround = 0;
  dSea = 1;
  dAir = 2;

  { model kinds }
  mkSelfDeveloped = $00;
  mkEnemyDeveloped = $01;
  mkSpecial_Boat = $08;
  mkSpecial_SubCabin = $0A;
  mkSpecial_TownGuard = $10;
  mkSpecial_Glider = $11;
  mkScout = $20;
  mkSlaves = $21;
  mkSettler = $22;
  mkCommando = $23;
  mkFreight = $24;

  { unit flags }
  unFortified = $01;
  unBombsLoaded = $02;
  unMountainDelay = $04;
  unConscripts = $08;
  unWithdrawn = $10;
  unMulti = $80;

  // unit report flags
  urfAlwaysSupport = $01;
  urfDeployed = $02;

  // unit moves
  umCapturing = $0100;
  umSpyMission = $0200;
  umBombarding = $0400;
  umExpelling = $0800;
  umShipLoading = $1000;
  umShipUnloading = $2000;
  umPlaneLoading = $4000;
  umPlaneUnloading = $8000;

  { model flags }
  mdZOC = $01;
  mdCivil = $02;
  mdDoubleSupport = $04;

  { player happened flags }
  phTech = $01;
  phStealTech = $02;
  phChangeGov = $08;
  phGliderLost = $100;
  phPlaneLost = $200;
  phPeaceViolation = $400;
  phPeaceEvacuation = $800;
  phShipComplete = $2000;
  phTimeUp = $4000;
  phExtinct = $8000;
  phGameEnd = $F000;

  { city happened flags }
  chDisorder = $01;
  chProduction = $02;
  chPopIncrease = $04;
  chPopDecrease = $08;
  chUnitLost = $10;
  chImprovementLost = $20;
  chProductionSabotaged = $40;
  chNoGrowthWarning = $80;
  chPollution = $100;
  chSiege = $200;
  chOldWonder = $400;
  chNoSettlerProd = $800;
  chFounded = $1000;
  chAfterCapture = $2000;
  chCaptured = $F0000;
  chImprovementSold = $80000000;

  { city info flags }
  ciCapital = $01;
  ciWalled = $02;
  ciCoastalFort = $04;
  ciMissileBat = $08;
  ciBunker = $10;
  ciSpacePort = $20;

  { city tile available values }
  faAvailable = 0;
  faNotAvailable = 1;
  faSiege = 2;
  faTreaty = 4;
  faInvalid = $FF;

  // battle history flags
  bhEnemyAttack = $01;
  bhMyUnitLost = $02;
  bhEnemyUnitLost = $04;

  { move advice special destinations }
  maNextCity = -1;

  { goverment forms }
  nGov = 8;
  gAnarchy = 0;
  gDespotism = 1;
  gMonarchy = 2;
  gRepublic = 3;
  gFundamentalism = 4;
  gCommunism = 5;
  gDemocracy = 6;
  gFuture = 7;

  // ship change reasons
  scrProduction = 0;
  scrDestruction = 1;
  scrTrade = 2;
  scrCapture = 3;

  { unit jobs }
  nJob = 15;
  jNone = 0;
  jRoad = 1;
  jRR = 2;
  jClear = 3;
  jIrr = 4;
  jFarm = 5;
  jAfforest = 6;
  jMine = 7;
  jCanal = 8;
  jTrans = 9;
  jFort = 10;
  jPoll = 11;
  jBase = 12;
  jPillage = 13;
  jCity = 14;

  // job preconditions are:
  // technology JobPreq is available, no city, plus the following:
  // jRoad: no river when bridge building unavailable
  // jRR: road
  // jClear: Terrain.ClearTerrain, Hanging Gardens for desert
  // jIrr: Terrain.IrrEff
  // jFarm: irrigation
  // jAfforest: Terrain.AfforestTerrain
  // jMine: Terrain.MineEff
  // jCanal: no Mountains, no Arctic
  // jTrans: Terrain.TransWork
  // jPoll: pollution
  // jPillage: any tile improvement
  // jCity, jFort, jBase: none

  // spy mission
  nSpyMission = 5;
  smSabotageProd = 0;
  smStealMap = 1;
  smStealForeignReports = 2;
  smStealCivilReport = 3;
  smStealMilReport = 4;

  // resource weights
  rwOff = $00000000;
  rwMaxGrowth = $3F514141; // 120*F + 1/8*P + 1/16*T + 1/16*S
  rwMaxProd = $413F1F01; // 1/16*F + 120*P + 30*T + 1*S
  rwMaxScience = $41040408; // 1/16*F + 4*P + 4*T + 8*S
  rwForceProd = $F1080201; // F^1/2 * (8*P + 2*T + 1*S)
  rwForceScience = $F1010101; // F^1/2 * (1*P + 1*T + 1*S)

  { advances }
  adAdvancedFlight = 0;
  adAmphibiousWarfare = 1;
  adAstronomy = 2;
  adAtomicTheory = 3;
  adAutomobile = 4;
  adBallistics = 5;
  adBanking = 6;
  adBridgeBuilding = 7;
  adBronzeWorking = 8;
  adCeremonialBurial = 9;
  adChemistry = 10;
  adChivalry = 11;
  adComposites = 12;
  adCodeOfLaws = 13;
  adCombinedArms = 14;
  adCombustionEngine = 15;
  adCommunism = 16;
  adComputers = 17;
  adConscription = 18;
  adConstruction = 19;
  adTheCorporation = 20;
  adSpaceFlight = 21;
  adCurrency = 22;
  adDemocracy = 23;
  adEconomics = 24;
  adElectricity = 25;
  adElectronics = 26;
  adEngineering = 27;
  adEnvironmentalism = 28;
  adWheel = 29;
  adExplosives = 30;
  adFlight = 31;
  adIntelligence = 32;
  adGunpowder = 33;
  adHorsebackRiding = 34;
  adImpulseDrive = 35;
  adIndustrialization = 36;
  adSmartWeapons = 37;
  adInvention = 38;
  adIronWorking = 39;
  adTheLaser = 40;
  adNuclearPower = 41;
  adLiterature = 42;
  adInternet = 43;
  adMagnetism = 44;
  adMapMaking = 45;
  adMasonry = 46;
  adMassProduction = 47;
  adMathematics = 48;
  adMedicine = 49;
  adMetallurgy = 50;
  adMin = 51;
  adMobileWarfare = 52;
  adMonarchy = 53;
  adMysticism = 54;
  adNavigation = 55;
  adNuclearFission = 56;
  adPhilosophy = 57;
  adPhysics = 58;
  adPlastics = 59;
  adPoetry = 60;
  adPottery = 61;
  adRadio = 62;
  adRecycling = 63;
  adRefrigeration = 64;
  adMonotheism = 65;
  adTheRepublic = 66;
  adRobotics = 67;
  adRocketry = 68;
  adRailroad = 69;
  adSanitation = 70;
  adScience = 71;
  adWriting = 72;
  adSeafaring = 73;
  adSelfContainedEnvironment = 74;
  adStealth = 75;
  adSteamEngine = 76;
  adSteel = 77;
  adSyntheticFood = 78;
  adTactics = 79;
  adTheology = 80;
  adTheoryOfGravity = 81;
  adTrade = 82;
  adTransstellarColonization = 83;
  adUniversity = 84;
  adAdvancedRocketry = 85;
  adWarriorCode = 86;
  adAlphabet = 87;
  adPolytheism = 88;
  adRefining = 89;
  futComputingTechnology = 90;
  futNanoTechnology = 91;
  futMaterialTechnology = 92;
  futArtificialIntelligence = 93;

  FutureTech = [futComputingTechnology, futNanoTechnology,
    futMaterialTechnology, futArtificialIntelligence];

  adMilitary = $800; // Military Research

  { wonders }
  woPyramids = 00;
  woZeus = 01;
  woGardens = 02;
  woColossus = 03;
  woLighthouse = 04;
  woGrLibrary = 05;
  woOracle = 06;
  woSun = 07;
  woLeo = 08;
  woMagellan = 09;
  woMich = 10; { 11; }
  woNewton = 12;
  woBach = 13;
  { 14; } woLiberty = 15;
  woEiffel = 16;
  woHoover = 17;
  woShinkansen = 18;
  woManhattan = 19;
  woMir = 20;

  { city improvements }
  imTrGoods = 28;
  imBarracks = 29;
  imGranary = 30;
  imTemple = 31;
  imMarket = 32;
  imLibrary = 33;
  imCourt = 34;
  imWalls = 35;
  imAqueduct = 36;
  imBank = 37;
  imCathedral = 38;
  imUniversity = 39;
  imHarbor = 40;
  imTheater = 41;
  imFactory = 42;
  imMfgPlant = 43;
  imRecycling = 44;
  imPower = 45;
  imHydro = 46;
  imNuclear = 47;
  imPlatform = 48;
  imTownHall = 49;
  imSewer = 50;
  imSupermarket = 51;
  imHighways = 52;
  imResLab = 53;
  imMissileBat = 54;
  imCoastalFort = 55;
  imAirport = 56;
  imDockyard = 57;
  imPalace = 58;
  imGrWall = 59;
  imColosseum = 60;
  imObservatory = 61;
  imMilAcademy = 62;
  imBunker = 63;
  imAlgae = 64;
  imStockEx = 65;
  imSpacePort = 66;
  imShipComp = 67;
  imShipPow = 68;
  imShipHab = 69;

  SettlerFood: array [0 .. nGov - 1] of Integer = (1, 1, 1, 2, 1, 2, 2, 2);
  CorrLevel: array [0 .. nGov - 1] of Integer = (3, 3, 1, 2, 1, 0, 0, 0);
  SupportFree: array [0 .. nGov - 1] of Integer = (2, 2, 1, 0, 2, 1, 0, 0);
  // in 1/2*city size

  // special prerequisite values
  preNone = -1;
  preLighthouse = -2;
  preSun = -3;
  preLeo = -4;
  preBuilder = -5;
  preNA = -$FF;

  JobPreq: array [0 .. nJob - 1] of Integer = (preNone, preNone, adRailroad,
    preNone, preNone, adRefrigeration, preNone, preNone, adExplosives,
    adExplosives, adConstruction, preNone, adMedicine, preNone, preNone);

  AdvPreq: array [0 .. nAdv - 1, 0 .. 2] of Integer = { advance prerequisites }
    ((adFlight, adRobotics, preNone), // adAdvancedFlight
    (adNavigation, adTactics, preNone), // adAmphibiousWarfare
    (adMysticism, adAlphabet, preNone), // adAstronomy
    (adTheoryOfGravity, preNone, preNone), // adAtomicTheory
    (adCombustionEngine, adSteel, preNone), // adAutomobile
    (adMathematics, adMetallurgy, preNone), // adBallistics
    (adCurrency, adEngineering, preNone), // adBanking
    (adConstruction, adWheel, preNone), // adBridgeBuilding
    (preNone, preNone, preNone), // adBronzeWorking
    (preNone, preNone, preNone), // adCeremonialBurial
    (adScience, preNone, preNone), // adChemistry
    (adMonarchy, adWarriorCode, preNone), // adChivalry
    (adMetallurgy, adPlastics, preNone), // adComposites
    (adWriting, preNone, preNone), // adCodeOfLaws
    (adAdvancedFlight, adMobileWarfare, preNone), // adCombinedArms
    (adRefining, adExplosives, preNone), // adCombustionEngine
    (adPhilosophy, adIndustrialization, preNone), // adCommunism
    (adMin, preNone, preNone), // adComputers
    (adTheRepublic, adTactics, preNone), // adConscription
    (adMasonry, adAlphabet, preNone), // adConstruction
    (adEconomics, adDemocracy, preNone), // adTheCorporation
    (adAdvancedFlight, adAdvancedRocketry, preNone), // adSpaceFlight
    (adBronzeWorking, preNone, preNone), // adCurrency
    (adConscription, adIndustrialization, preNone), // adDemocracy
    (adBanking, adUniversity, preNone), // adEconomics
    (adMagnetism, preNone, preNone), // adElectricity
    (adRadio, adAtomicTheory, preNone), // adElectronics
    (adConstruction, adBronzeWorking, preNone), // adEngineering
    (adIndustrialization, preNone, preNone), // adEnvironmentalism
    (preNone, preNone, preNone), // adWheel
    (adChemistry, adEngineering, preNone), // adExplosives
    (adCombustionEngine, adPhysics, preNone), // adFlight
    (adTactics, adInvention, preNone), // adIntelligence
    (adMedicine, adIronWorking, preNone), // adGunpowder
    (preNone, preNone, preNone), // adHorsebackRiding
    (adSpaceFlight, adNuclearPower, preNone), // adImpulseDrive
    (adRailroad, adBanking, preNone), // adIndustrialization
    (adAdvancedRocketry, adTheLaser, preNone), // adIntelligenArms
    (adWriting, adWheel, preNone), // adInvention
    (adBronzeWorking, adInvention, preNone), // adIronWorking
    (adMin, adPhysics, preNone), // adTheLaser
    (adNuclearFission, preNone, preNone), // adNuclearPower
    (adPoetry, adTrade, preNone), // adLiterature
    (adDemocracy, adComputers, preNone), // adLybertarianism
    (adPhysics, adIronWorking, preNone), // adMagnetism
    (adAlphabet, preNone, preNone), // adMapMaking
    (preNone, preNone, preNone), // adMasonry
    (adAutomobile, adElectronics, adTheCorporation), // adMassProduction
    (adCurrency, adAlphabet, preNone), // adMathematics
    (adMysticism, adPottery, preNone), // adMedicine
    (adGunpowder, preNone, preNone), // adMetallurgy
    (adRobotics, adPlastics, preNone), // adMin
    (adAutomobile, adTactics, preNone), // adMobileWarfare
    (adPolytheism, preNone, preNone), // adMonarchy
    (adCeremonialBurial, preNone, preNone), // adMysticism
    (adSeafaring, adAstronomy, preNone), // adNavigation
    (adAtomicTheory, adMassProduction, preNone), // adNuclearFission
    (adMathematics, adLiterature, preNone), // adPhilosophy
    (adScience, preNone, preNone), // adPhysics
    (adMassProduction, adRefining, preNone), // adPlastics
    (adMysticism, adWarriorCode, preNone), // adPoetry
    (preNone, preNone, preNone), // adPottery
    (adElectricity, adEngineering, preNone), // adRadio
    (adEnvironmentalism, adPlastics, preNone), // adRecycling
    (adElectricity, preNone, preNone), // adRefrigeration
    (adPolytheism, adAstronomy, preNone), // adMonotheism
    (adLiterature, preNone, preNone), // adTheRepublic
    (adMassProduction, adEconomics, preNone), // adRobotics
    (adBallistics, adExplosives, preNone), // adRocketry
    (adSteamEngine, adBridgeBuilding, preNone), // adRailroad
    (adEnvironmentalism, adMedicine, preNone), // adSanitation
    (adMetallurgy, adTheology, adPhilosophy), // adScience
    (adAlphabet, preNone, preNone), // adWriting
    (adPottery, adMapMaking, preNone), // adSeafaring
    (adRecycling, adSyntheticFood, preNone), // adSelfContainedEnvironment
    (adComposites, adRadio, preNone), // adStealth
    (adScience, adEngineering, preNone), // adSteamEngine
    (adIronWorking, adRailroad, preNone), // adSteel
    (adChemistry, adRefrigeration, preNone), // adSyntheticFood
    (adWarriorCode, adUniversity, preNone), // adTactics
    (adMonotheism, adPoetry, preNone), // adTheology
    (adAstronomy, adPhysics, preNone), // adTheoryOfGravity
    (adCurrency, adCodeOfLaws, preNone), // adTrade
    (adImpulseDrive, adSelfContainedEnvironment, preNone),
    // adTransstellarColonization
    (adScience, preNone, preNone), // adUniversity
    (adComputers, adRocketry, preNone), // adAdvancedRocketry
    (preNone, preNone, preNone), // adWarriorCode
    (preNone, preNone, preNone), // adAlphabet
    (adCeremonialBurial, adHorsebackRiding, preNone), // adPolytheism
    (adChemistry, preNone, preNone), // adRefining
    (adComputers, preNone, preNone), // futResearchTechnology
    (adRobotics, preNone, preNone), // futProductionTechnology
    (adComposites, preNone, preNone), // futArmorTechnology
    (adSmartWeapons, preNone, preNone)); // futMissileTechnology

Imp: array [0 .. nImp - 1] of // city improvements
record
  Kind: Integer;
  Preq: Integer;
  Cost: Integer;
  Maint: Integer;
  Expiration: Integer;
end
= ((Kind: ikWonder; Preq: adMathematics; Cost: 400; Maint: 0;
  Expiration: adDemocracy), // woPyramids
  (Kind: ikWonder; Preq: adPolytheism; Cost: 200; Maint: 0;
  Expiration: adElectronics), // woZeus
  (Kind: ikWonder; Preq: adInvention; Cost: 200; Maint: 0;
  Expiration: adNuclearFission), // woGardens
  (Kind: ikWonder; Preq: adBronzeWorking; Cost: 200; Maint: 0; Expiration: - 1),
  // woColossus
  (Kind: ikWonder; Preq: adMapMaking; Cost: 200; Maint: 0; Expiration: adSteel),
  // woLighthouse
  (Kind: ikWonder; Preq: adLiterature; Cost: 400; Maint: 0;
  Expiration: adPlastics), // woGrLibrary
  (Kind: ikWonder; Preq: adMysticism; Cost: 200; Maint: 0; Expiration: - 1),
  // woOracle
  (Kind: ikWonder; Preq: adChivalry; Cost: 300; Maint: 0;
  Expiration: adSpaceFlight), // woSun
  (Kind: ikWonder; Preq: adPhilosophy; Cost: 500; Maint: 0; Expiration: - 1),
  // woLeo
  (Kind: ikWonder; Preq: adNavigation; Cost: 300; Maint: 0; Expiration: - 1),
  // woMagellan
  (Kind: ikWonder; Preq: adMonotheism; Cost: 400; Maint: 0; Expiration: - 1),
  // woMich
  (Kind: ikNA; Preq: preNA; Cost: 0; Maint: 0; Expiration: 0), // {11}
  (Kind: ikWonder; Preq: adTheoryOfGravity; Cost: 400; Maint: 0; Expiration: - 1), // woNewton
  (Kind: ikWonder; Preq: adTheology; Cost: 400; Maint: 0; Expiration: - 1),
  // woBach
  (Kind: ikNA; Preq: preNA; Cost: 0; Maint: 0; Expiration: 0), // {14}
  (Kind: ikWonder; Preq: adDemocracy; Cost: 500; Maint: 0; Expiration: - 1),
  // woLiberty
  (Kind: ikWonder; Preq: adSteel; Cost: 800; Maint: 0; Expiration: - 1),
  // woEiffel
  (Kind: ikWonder; Preq: adElectronics; Cost: 800; Maint: 0; Expiration: - 1),
  // woHoover
  (Kind: ikWonder; Preq: adPlastics; Cost: 500; Maint: 0; Expiration: - 1),
  // woShinkansen
  (Kind: ikWonder; Preq: adNuclearFission; Cost: 400; Maint: 0;
  Expiration: - 1), // woManhattan
  (Kind: ikWonder; Preq: adSpaceFlight; Cost: 800; Maint: 0; Expiration: - 1),
  // woMir
  (Kind: ikNA; Preq: preNA; Cost: 0; Maint: 0; Expiration: 0), // {21}
  (Kind: ikNA; Preq: preNA; Cost: 0; Maint: 0; Expiration: 0), // {22}
  (Kind: ikNA; Preq: preNA; Cost: 0; Maint: 0; Expiration: 0), // {23}
  (Kind: ikNA; Preq: preNA; Cost: 0; Maint: 0; Expiration: 0), // {24}
  (Kind: ikNA; Preq: preNA; Cost: 0; Maint: 0; Expiration: 0), // {25}
  (Kind: ikNA; Preq: preNA; Cost: 0; Maint: 0; Expiration: 0), // {26}
  (Kind: ikNA; Preq: preNA; Cost: 0; Maint: 0; Expiration: 0), // {27}
  (Kind: ikTrGoods; Preq: preNone; Cost: 0; Maint: 0; Expiration: 0), // imTrGoods
  (Kind: ikCommon; Preq: adWarriorCode; Cost: 40; Maint: 1; Expiration: 0), // imBarracks
  (Kind: ikCommon; Preq: adPottery; Cost: 60; Maint: 1; Expiration: 0), // imGranary
  (Kind: ikCommon; Preq: adCeremonialBurial; Cost: 40; Maint: 1; Expiration: 0), // imTemple
  (Kind: ikCommon; Preq: adCurrency; Cost: 60; Maint: 1; Expiration: 0), // imMarket
  (Kind: ikCommon; Preq: adWriting; Cost: 80; Maint: 3; Expiration: 0), // imLibrary
  (Kind: ikCommon; Preq: adCodeOfLaws; Cost: 80; Maint: 2; Expiration: 0), // imCourt
  (Kind: ikCommon; Preq: adMasonry; Cost: 80; Maint: 1; Expiration: 0), // imWalls
  (Kind: ikCommon; Preq: adConstruction; Cost: 80; Maint: 1; Expiration: 0), // imAqueduct
  (Kind: ikCommon; Preq: adBanking; Cost: 120; Maint: 2; Expiration: 0), // imBank
  (Kind: ikCommon; Preq: adMonotheism; Cost: 100; Maint: 1; Expiration: 0), // imCathedral
  (Kind: ikCommon; Preq: adUniversity; Cost: 160; Maint: 5; Expiration: 0), // imUniversity
  (Kind: ikCommon; Preq: adSeafaring; Cost: 60; Maint: 1; Expiration: 0), // imHarbor
  (Kind: ikCommon; Preq: adPoetry; Cost: 60; Maint: 2; Expiration: 0), // imTheater
  (Kind: ikCommon; Preq: adIndustrialization; Cost: 200; Maint: 3; Expiration: 0), // imFactory
  (Kind: ikCommon; Preq: adRobotics; Cost: 320; Maint: 5; Expiration: 0), // imMfgPlant
  (Kind: ikCommon; Preq: adRecycling; Cost: 320; Maint: 4; Expiration: 0), // imRecycling
  (Kind: ikCommon; Preq: adElectricity; Cost: 120; Maint: 2; Expiration: 0), // imPower
  (Kind: ikCommon; Preq: adEnvironmentalism; Cost: 120; Maint: 1; Expiration: 0), // imHydro
  (Kind: ikCommon; Preq: adNuclearPower; Cost: 240; Maint: 2; Expiration: 0), // imNuclear
  (Kind: ikCommon; Preq: adRefining; Cost: 160; Maint: 2; Expiration: 0), // imPlatform
  (Kind: ikCommon; Preq: preNone; Cost: 40; Maint: 1; Expiration: 0), // imTownHall
  (Kind: ikCommon; Preq: adSanitation; Cost: 120; Maint: 2; Expiration: 0), // imSewer
  (Kind: ikCommon; Preq: adRefrigeration; Cost: 80; Maint: 2; Expiration: 0), // imSupermarket
  (Kind: ikCommon; Preq: adAutomobile; Cost: 160; Maint: 4; Expiration: 0), // imHighways
  (Kind: ikCommon; Preq: adComputers; Cost: 240; Maint: 7; Expiration: 0), // imResLab
  (Kind: ikCommon; Preq: adAdvancedRocketry; Cost: 100; Maint: 1; Expiration: 0),
  // imMissileBat
  (Kind: ikCommon; Preq: adMetallurgy; Cost: 80; Maint: 1; Expiration: 0), // imCoastalFort
  (Kind: ikCommon; Preq: adAdvancedFlight; Cost: 160; Maint: 1; Expiration: 0), // imAirport
  (Kind: ikCommon; Preq: adAmphibiousWarfare; Cost: 80; Maint: 1; Expiration: 0), // imDockyard
  (Kind: ikNatLocal; Preq: preNone; Cost: 100; Maint: 0; Expiration: 0), // imPalace
  (Kind: ikNatLocal; Preq: adEngineering; Cost: 400; Maint: 4; Expiration: 0), // imGrWall
  (Kind: ikNatLocal; Preq: adConstruction; Cost: 200; Maint: 4; Expiration: 0), // imColosseum
  (Kind: ikNatLocal; Preq: adAstronomy; Cost: 300; Maint: 4; Expiration: 0), // imObservatory
  (Kind: ikNatLocal; Preq: adTactics; Cost: 100; Maint: 4; Expiration: 0), // imMilAcademy
  (Kind: ikNatLocal; Preq: adSteel; Cost: 200; Maint: 2; Expiration: 0), // imBunker
  (Kind: ikNatLocal; Preq: adSyntheticFood; Cost: 120; Maint: 2; Expiration: 0), // imAlgae
  (Kind: ikNatGlobal; Preq: adTheCorporation; Cost: 320; Maint: 4; Expiration: 0), // imStockEx
  (Kind: ikNatLocal; Preq: adSpaceFlight; Cost: 400; Maint: 0; Expiration: 0), // imSpacePort
  (Kind: ikShipPart; Preq: adTransstellarColonization; Cost: 240; Maint: 0; Expiration: 0),
  // imShipComp
  (Kind: ikShipPart; Preq: adImpulseDrive; Cost: 600; Maint: 0; Expiration: 0), // imShipPow
  (Kind: ikShipPart; Preq: adSelfContainedEnvironment; Cost: 800; Maint: 0; Expiration: 0));
// imShipHab

nImpReplacement = 5;
ImpReplacement: array [0 .. nImpReplacement - 1] of record
  NewImp: Integer;
  OldImp: Integer;
end
= ((NewImp: imSewer; OldImp: imAqueduct), (NewImp: imCourt; OldImp: imTownHall),
  (NewImp: imPalace; OldImp: imTownHall), (NewImp: imPalace; OldImp: imCourt),
  (NewImp: imMilAcademy; OldImp: imBarracks));

// colony ship
nShipPart = 3;
spComp = 0;
spPow = 1;
spHab = 2;
ShipNeed: array [0 .. nShipPart - 1] of Integer = (6, 4, 2);
ShipImpIndex: array [0 .. nShipPart - 1] of Integer = (imShipComp, imShipPow, imShipHab);

GovPreq: array [1 .. nGov - 1] of Integer = { government prerequisites }
  (preNone, adMonarchy, adTheRepublic, adTheology, adCommunism, adDemocracy,
  adInternet);

AgePreq: array [1 .. 3] of Integer = (adScience, adMassProduction,
  adTransstellarColonization);

Terrain: array [0 .. 11] of record
  MoveCost: Integer;
  Defense: Integer;
  ClearTerrain: Integer;
  IrrEff: Integer;
  IrrClearWork: Integer;
  AfforestTerrain: Integer;
  MineEff: Integer;
  MineAfforestWork: Integer;
  TransTerrain: Integer;
  TransWork: Integer;
  FoodRes: array [0 .. 2] of Integer;
  ProdRes: array [0 .. 2] of Integer;
  TradeRes: array [0 .. 2] of Integer;
  Filler: array [0 .. 12] of Integer;
end
= ((MoveCost: 1; Defense: 4; ClearTerrain: - 1; IrrEff: 0; IrrClearWork: 0;
  AfforestTerrain: - 1; MineEff: 0; MineAfforestWork: 0; TransTerrain: - 1;
  TransWork: 0; FoodRes: (0, 0, 0); ProdRes: (0, 0, 0);
  TradeRes: (0, 0, 0); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Ocn }
  (MoveCost: 1; Defense: 4; ClearTerrain: - 1; IrrEff: 0; IrrClearWork: 0;
  AfforestTerrain: - 1; MineEff: 0; MineAfforestWork: 0; TransTerrain: - 1;
  TransWork: 0; FoodRes: (1, 5, 1); ProdRes: (0, 0, 5);
  TradeRes: (3, 3, 3); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Sho }
  (MoveCost: 1; Defense: 4; ClearTerrain: - 1; IrrEff: 1; IrrClearWork: 600;
  AfforestTerrain: fForest; MineEff: 0; MineAfforestWork: 1800;
  TransTerrain: fHills; TransWork: 3000; FoodRes: (3, 2, 2); ProdRes: (0, 1, 0);
  TradeRes: (1, 1, 1); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Gra }
  (MoveCost: 1; Defense: 4; ClearTerrain: fGrass; IrrEff: 0; IrrClearWork: 1800;
  AfforestTerrain: - 1; MineEff: 1; MineAfforestWork: 600;
  TransTerrain: fPrairie; TransWork: 3000; FoodRes: (0, 3, 0);
  ProdRes: (1, 1, 4); TradeRes: (1, 1, 1); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Dst }
  (MoveCost: 1; Defense: 4; ClearTerrain: - 1; IrrEff: 1; IrrClearWork: 600;
  AfforestTerrain: fForest; MineEff: 0; MineAfforestWork: 2400;
  TransTerrain: - 1; TransWork: 0; FoodRes: (1, 3, 1); ProdRes: (1, 1, 3);
  TradeRes: (1, 1, 1); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Pra }
  (MoveCost: 1; Defense: 4; ClearTerrain: - 1; IrrEff: 1; IrrClearWork: 600;
  AfforestTerrain: - 1; MineEff: 0; MineAfforestWork: 0; TransTerrain: fGrass;
  TransWork: 3000; FoodRes: (1, 1, 1); ProdRes: (0, 0, 4);
  TradeRes: (1, 6, 1); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Tun }
  (MoveCost: 2; Defense: 4; ClearTerrain: - 1; IrrEff: 0; IrrClearWork: 0;
  AfforestTerrain: - 1; MineEff: 3; MineAfforestWork: 1800; TransTerrain: - 1;
  TransWork: 0; FoodRes: (0, 3, 0); ProdRes: (1, 1, 0);
  TradeRes: (0, 4, 0); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Arc }
  (MoveCost: 2; Defense: 6; ClearTerrain: fGrass; IrrEff: 0; IrrClearWork: 2400;
  AfforestTerrain: fForest; MineEff: 0; MineAfforestWork: 2400;
  TransTerrain: fHills; TransWork: 3000; FoodRes: (1, 1, 1); ProdRes: (0, 4, 1);
  TradeRes: (1, 1, 5); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Swa }
  (MoveCost: 0; Defense: 0; ClearTerrain: 0; IrrEff: 0;
  IrrClearWork: 0; AfforestTerrain: 0; MineEff: 0; MineAfforestWork: 0;
  TransTerrain: 0; TransWork: 0; FoodRes: (0, 0, 0); ProdRes: (0, 0, 0);
  TradeRes: (0, 0, 0); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { - }
  (MoveCost: 2; Defense: 6; ClearTerrain: fPrairie; IrrEff: 0;
  IrrClearWork: 600; AfforestTerrain: - 1; MineEff: 0; MineAfforestWork: 0;
  TransTerrain: - 1; TransWork: 0; FoodRes: (1, 3, 1); ProdRes: (2, 2, 2);
  TradeRes: (1, 1, 4); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { For }
  (MoveCost: 2; Defense: 8; ClearTerrain: - 1; IrrEff: 1; IrrClearWork: 600;
  AfforestTerrain: - 1; MineEff: 3; MineAfforestWork: 1200;
  TransTerrain: fGrass; TransWork: 6000; FoodRes: (1, 1, 1); ProdRes: (0, 0, 2);
  TradeRes: (0, 4, 0); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Hil }
  (MoveCost: 3; Defense: 12; ClearTerrain: - 1; IrrEff: 0; IrrClearWork: 0;
  AfforestTerrain: - 1; MineEff: 2; MineAfforestWork: 1200; TransTerrain: - 1;
  TransWork: 0; FoodRes: (0, 0, 0); ProdRes: (1, 4, 1);
  TradeRes: (0, 0, 7); Filler: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))); { Mou }

// settler work required MP
PillageWork = 100;
CityWork = 900;
FarmWork = 3; // *IrrClearWork
RoadWork = 300; // *MoveCost
RoadBridgeWork = 900;
RRWork = 600; // *MoveCost
RRBridgeWork = 900;
CanalWork = 1800;
FortWork = 600; // *MoveCost
BaseWork = 600; // *MoveCost
PollWork = 1800;

// upgrades for new unit models
// upgrade[domain,0].preq is domain precondition advance
// cost values accumulate if prerequisite is future tech / are maximized if not
nUpgrade = 15;
upgrade: array [0 .. nDomains - 1, 0 .. nUpgrade - 1] of record
  Preq: Integer;
  Strength: Integer;
  Trans: Integer;
  Cost: Integer;
end
= (((Preq: adWarriorCode; Strength: 4; Trans: 0; Cost: 3),
  (Preq: adBronzeWorking; Strength: 2; Trans: 0; Cost: 4),
  (Preq: adIronWorking; Strength: 2; Trans: 0; Cost: 5),
  (Preq: adChivalry; Strength: 2; Trans: 0; Cost: 5),
  (Preq: adMonotheism; Strength: 3; Trans: 0; Cost: 7),
  (Preq: adGunpowder; Strength: 3; Trans: 0; Cost: 8),
  (Preq: adExplosives; Strength: 4; Trans: 0; Cost: 9),
  (Preq: adTactics; Strength: 5; Trans: 0; Cost: 10),
  (Preq: adRadio; Strength: 6; Trans: 0; Cost: 11),
  (Preq: adDemocracy; Strength: 6; Trans: 0; Cost: 5),
  (Preq: adMobileWarfare; Strength: 7; Trans: 0; Cost: 12),
  (Preq: adRobotics; Strength: 8; Trans: 0; Cost: 15),
  (Preq: adComposites; Strength: 8; Trans: 0; Cost: 15),
  (Preq: adTheLaser; Strength: 8; Trans: 0; Cost: 14),
  (Preq: futMaterialTechnology; Strength: 10; Trans: 0; Cost: 2)),
  ((Preq: adMapMaking; Strength: 4; Trans: 1; Cost: 8),
  (Preq: adNavigation; Strength: 4; Trans: 0; Cost: 10),
  (Preq: adEngineering; Strength: 0; Trans: 1; Cost: 8),
  (Preq: adGunpowder; Strength: 8; Trans: 0; Cost: 12),
  (Preq: adMagnetism; Strength: 12; Trans: 1; Cost: 20),
  (Preq: adExplosives; Strength: 16; Trans: 0; Cost: 24),
  (Preq: adSteamEngine; Strength: 24; Trans: 0; Cost: 28),
  (Preq: adAmphibiousWarfare; Strength: 24; Trans: 1; Cost: 18),
  (Preq: adAdvancedRocketry; Strength: 32; Trans: 0; Cost: 38),
  (Preq: futMaterialTechnology; Strength: 14; Trans: 0; Cost: 4),
  (Preq: futArtificialIntelligence; Strength: 14; Trans: 0; Cost: 4),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0)),
  ((Preq: adFlight; Strength: 12; Trans: 1; Cost: 14),
  (Preq: adTactics; Strength: 6; Trans: 0; Cost: 17),
  (Preq: adElectronics; Strength: 6; Trans: 0; Cost: 20),
  (Preq: adMin; Strength: 8; Trans: 0; Cost: 24),
  (Preq: adComposites; Strength: 8; Trans: 0; Cost: 26),
  (Preq: adSmartWeapons; Strength: 11; Trans: 0; Cost: 32),
  (Preq: futArtificialIntelligence; Strength: 7; Trans: 0; Cost: 4),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0),
  (Preq: preNA; Strength: 0; Trans: 0; Cost: 0)));

{ model features }
nFeature = 27;
mcWeapons = 0;
mcArmor = 1;
mcMob = 2;
mcSeaTrans = 3;
mcCarrier = 4;
mcTurbines = 5;
mcBombs = 6;
mcFuel = 7;
mcAirTrans = 8;
mcNav = 9;
mcRadar = 10;
mcSub = 11;
mcArtillery = 12;
mcAlpine = 13;
mcSupplyShip = 14;
mcOver = 15;
mcAirDef = 16;
mcSpy = 17;
mcSE = 18;
mcNP = 19;
mcJet = 20;
mcStealth = 21;
mcFanatic = 22;
mcFirst = 23;
mcWill = 24;
mcAcademy = 25;
mcLine = 26;
mcFirstNonCap = mcNav;
AutoFeature: set of mcFirstNonCap .. nFeature - 1 = [mcNav, mcSE, mcNP, mcJet,
  mcAcademy];
// unit class advances, automatically applied if available

Feature: array [0 .. nFeature - 1] of { unit model features }
  record
    Domains: Integer;
    Preq: Integer;
    Weight: Integer;
    Cost: Integer;
  end
= ((Domains: 7; Preq: preNone; Weight: 1; Cost: 1), { mcOffense }
  (Domains: 7; Preq: preNone; Weight: 1; Cost: 1), { mcDefense }
  (Domains: 1; Preq: adHorsebackRiding; Weight: 1; Cost: 1), { mcMob }
  (Domains: 2; Preq: preNone; Weight: 2; Cost: 1), { mcSeaTrans }
  (Domains: 2; Preq: adAdvancedFlight; Weight: 2; Cost: 2), { mcCarrier }
  (Domains: 2; Preq: adPhysics; Weight: 3; Cost: 1), { mcTurbines }
  (Domains: 4; Preq: adAdvancedFlight; Weight: 1; Cost: 1), { mcBombs }
  (Domains: 4; Preq: preNone; Weight: 1; Cost: 1), { mcFuel }
  (Domains: 4; Preq: adCombinedArms; Weight: 2; Cost: 1), { mcAirTrans }
  (Domains: 2; Preq: adNavigation; Weight: 0; Cost: 0), { mcNav }
  (Domains: 2; Preq: adRadio; Weight: 0; Cost: 1), { mcRadar }
  (Domains: 2; Preq: adCombustionEngine; Weight: 2; Cost: 1), { mcSub }
  (Domains: 3; Preq: adBallistics; Weight: 1; Cost: 1), { mcArtillery }
  (Domains: 1; Preq: adTactics; Weight: 2; Cost: 1), { mcAlpine }
  (Domains: 2; Preq: adMedicine; Weight: 1; Cost: 1), { mcSupplyShip }
  (Domains: 1; Preq: adBridgeBuilding; Weight: 0; Cost: 2), { mcOver }
  (Domains: 2; Preq: adAdvancedRocketry; Weight: 1; Cost: 1), { mcAirDef }
  (Domains: 4; Preq: adIntelligence; Weight: 2; Cost: 1), { mcSpy }
  (Domains: 2; Preq: adSteamEngine; Weight: 0; Cost: 0), { mcSE }
  (Domains: 2; Preq: adNuclearPower; Weight: 0; Cost: 0), { mcNP }
  (Domains: 4; Preq: adRocketry; Weight: 0; Cost: 0), { mcJet }
  (Domains: 4; Preq: adStealth; Weight: 1; Cost: 2), { mcStealth }
  (Domains: 5; Preq: adCommunism; Weight: 0; Cost: 1), { mcFanatic }
  (Domains: 1; Preq: preSun; Weight: 0; Cost: 1), { mcFirst }
  (Domains: 1; Preq: preSun; Weight: 0; Cost: 1), { mcWill }
  (Domains: 1; Preq: preSun; Weight: 0; Cost: 0), { mcAcademy }
  (Domains: 7; Preq: adMassProduction; Weight: 0; Cost: 0)); { mcLine }

WeightPreq7: array [0 .. nDomains - 1] of Integer = (adHorsebackRiding, adSeafaring,
  adAdvancedFlight);
WeightPreq10: array [0 .. nDomains - 1] of Integer = (adAutomobile, adSteel, preNA);

INFIN = 999999;

// for backward compatibility
fRare = fDeadLands;
fRare1 = fCobalt;
fRare2 = fUranium;
mkCaravan = mkFreight;
mkDiplomat = mkCommando;
gLybertarianism = gFuture;
trCeaseFire = 1;
adIntelligenArms = adSmartWeapons;
adIntelligentArms = adSmartWeapons;
adRadioCommunication = adRadio;
adLybertarianism = adInternet;
futResearchTechnology = futComputingTechnology;
futProductionTechnology = futNanoTechnology;
futArmorTechnology = futMaterialTechnology;
futMissileTechnology = futArtificialIntelligence;
imNatObs = imObservatory;
imElite = imMilAcademy;
mcOffense = mcWeapons;
mcDefense = mcArmor;
mcLongRange = mcArtillery;
mcHospital = mcSupplyShip;

// Wonders CityID constants
WonderNotBuiltYet = -1;
WonderDestroyed = -2;

type
  TServerCall = function (Command, Player, Subject: Integer; var Data)
    : Integer; stdcall;
  TClientCall = procedure (Command, Player: Integer; var Data); stdcall;

  TCommand = (
    cmInitModule = $0000,
    cmReleaseModule = $0100,
    cmBroadcast = $0200,
    cmHelpOnly = $0700,
    cmStartHelp = $0710,
    cmStartCredits = $0720,

    cmNewGame = $0800,
    cmLoadGame = $0810,
    cmMovie = $0820,
    cmNewGameEx = $0840,
    cmLoadGameEx = $0850,
    cmNewMap = $0880,
    cmReplay = $08E0,
    cmGetReady = $08F0,
    cmBreakGame = $0900,

    cmTurn = $2000,
    cmResume = $2010,
    cmContinue = $2080,
    cmMovieTurn = $2100,
    cmMovieEndTurn = $2110,
    cmEditMap = $2800,

    // cShowTileM=$3000;cShowTileA=$3010;cShowFoundCity=$3020;
    cmShowUnitChanged = $3030,
    cmShowAfterMove = $3040,
    cmShowAfterAttack = $3050,
    cmShowCityChanged = $3090,
    // cShowMove=$3100;cShowCapture=$3110;
    // cShowAttackBegin=$3200;cShowAttackWon=$3210;cShowAttackLost=$3220;
    cmShowMoving = $3140,
    cmShowCapturing = $3150,
    cmShowAttacking = $3240,
    cmShowMissionResult = $3300,
    cmShowShipChange = $3400,
    cmShowGreatLibTech = $3500,
    cmShowTurnChange = $3700,
    cmShowCancelTreaty = $3800,
    cmShowEndContact = $3810,
    cmShowCancelTreatyByAlliance = $3820,
    cmShowSupportAllianceAgainst = $3830,
    cmShowPeaceViolation = $3880,
    cmShowGame = $3F00, { cShowSuperView=$3F80; }
    cmRefreshDebugMap = $3F90,

    // diplomacy commands equal to server, see below

    cmDebugMessage = $7000,
    cmShowNego = $7010
  );

  TUn = packed record
    Loc: LongInt; { location }
    Status: LongInt; // free for AI use
    SavedStatus: LongInt; // for server internal use only
    ID: Word; // unit number, never changes, unique within this nation
    mix: SmallInt; { model index }
    Home: SmallInt; { home city index, -1 if none }
    Master: SmallInt; { index of transporting unit, -1 if none }
    Movement: SmallInt; { movement left for this turn }
    Health: ShortInt; // = 100-Damage
    Fuel: ShortInt;
    Job: Byte; { current terrain improvement job }
    Exp: Byte; { micro experience, the level is Exp div ExpCost }
    TroopLoad: Byte; { number of transported ground units }
    AirLoad: Byte; // number of transported air units
    Flags: Cardinal;
  end;
  PUn = ^TUn;

  { TCity }

  TCity = packed record
    Loc: LongInt; { location }
    Status: LongInt; // free for AI use
    SavedStatus: LongInt; // for server internal use only
    ID: Word; // founding player shl 12 + number, never changes, unique within the whole game
    Size: Word;
    Project: SmallInt; // current production project, see city project flags
    Project0: SmallInt; // for server use only
    Food: SmallInt; // collected food in storage
    Pollution: SmallInt; // collected pollution in dump
    Prod: SmallInt; // for project collected production points
    Prod0: SmallInt;
    // for project collected production points in the beginning of the turn
    Flags: Cardinal; // what happened within the last turnaround
    Tiles: Cardinal; { currently by city exploited tiles, bitset with index
      (dy+3) shl 2+(dx+3) shr 1, (dx,dy) relative to central tile }
    N1: Cardinal; // reserved for future use
    Built: array [0 .. (nImp + 3) div 4 * 4 - 1] of ShortInt;
    // array value =1 indicates built improvement
  end;
  PCity = ^TCity;

  TModel = packed record
    Status: LongInt; // free for AI use
    SavedStatus: LongInt; // for server internal use only
    ID: Word; // developing player shl 12 + number, never changes, unique within the whole game
    IntroTurn: Word;
    Built: Word; // units built with this model
    Lost: Word; // units of this model lost in combat
    Kind: Byte;
    Domain: Byte;
    Attack: Word;
    Defense: Word;
    Speed: Word;
    Cost: Word;
    MStrength: Word;
    // construction time multipliers, only valid if kind is mkSelfDeveloped or mkEnemyDeveloped
    MTrans: Byte;
    MCost: Byte;
    Weight: Byte;
    MaxWeight: Byte;
    // weight and maximum weight (construction time)
    Upgrades: Cardinal; // bitarray indicating all upgrades
    Flags: Cardinal;
    Cap: array [0 .. (nFeature + 3) div 4 * 4 - 1] of Byte; // special features
  end;

  TUnitInfo = packed record
    Loc: LongInt;
    mix: Word; // index of unit model for its owner
    emix: Word; // index in enemy model list
    Owner: Byte;
    Health: ShortInt; // = 100-Damage
    Fuel: ShortInt;
    Job: Byte; // current terrain improvement job
    Exp: Byte; { micro experience, the level is Exp div ExpCost }
    Load: Byte; { number of transported units }
    Flags: Word;
  end;
  PUnitInfo = ^TUnitInfo;

  TCityInfo = packed record
    Loc: LongInt;
    Status: LongInt; // free for AI use
    SavedStatus: LongInt; // for server internal use only
    Owner: Word; // last known owner, even if not alive anymore!
    ID: Word; // founding player <<12 + number, never changes, unique within the whole game
    Size: Word;
    Flags: Word;
  end;
  PCityInfo = ^TCityInfo;

  TModelInfo = packed record
    Owner: Word; // Player which owns the model
    mix: Word; // index of unit model for its owner
    ID: Word; // developing player shl 12 + number, never changes, unique within the whole game
    Kind: Byte;
    Domain: Byte;
    Attack: Word;
    Defense: Word;
    Speed: Word;
    Cost: Word;
    TTrans: Byte; // ground unit transport capability
    ATrans_Fuel: Byte; // air unit transport capability resp. fuel
    Bombs: Word; // additional attack with bombs
    Cap: Cardinal; // special features, bitset with index Feature-mcFirstNonCap
    MaxUpgrade: Byte; // maximum used upgrade
    Weight: Byte;
    Lost: Word;
  end;

  TBattle = packed record
    Enemy: Byte;
    Flags: Byte;
    Turn: Word;
    mix: Word;
    mixEnemy: Word;
    ToLoc: Integer;
    FromLoc: Integer;
  end;

  TWonderInfo = record
    CityID: Integer; // -2 if destroyed, -1 if never completed, >=0 ID of city
    EffectiveOwner: Integer;
    // owning player if effective, -1 if expired or not built
  end;

  TShipInfo = record
    Parts: array [0 .. nShipPart - 1] of Integer;
  end;

  TEnemyReport = record
    TurnOfContact: Integer;
    TurnOfCivilReport: Integer;
    TurnOfMilReport: Integer;
    Attitude: Integer;
    Credibility: Integer; // 0..100, last update: ToC
    Treaty: array [0 .. nPl - 1] of Integer;
    // diplomatic status with other nations, last update: ToCR
    Government: Integer; // gAnarchy..gDemocracy, last update: ToCR
    Money: Integer; // last update: ToCR
    ResearchTech: Integer; // last update: ToCR
    ResearchDone: Integer; // last update: ToCR
    Tech: array [0 .. (nAdv + 3) div 4 * 4 - 1] of ShortInt;
    // tech status indicator, last update: ToCR
    nModelCounted: Integer;
    // number of models with info in UnCount, last update: ToMR
    UnCount: array [0 .. INFIN] of Word;
    // number of available units for each model, last update: ToMR
  end;

  TMoveAdviceData = record
    ToLoc: Integer;
    nStep: Integer;
    MoreTurns: Integer;
    MaxHostile_MovementLeft: Integer;
    dx: array [0 .. 24] of Integer;
    dy: array [0 .. 24] of Integer;
  end;

  TPlaneReturnData = record
    Loc: Integer;
    Fuel: Integer;
    Movement: Integer;
  end;

  TTileInfo = record
    Food: Integer;
    Prod: Integer;
    Trade: Integer;
    ExplCity: Integer;
  end;

  TCityReport = record
    HypoTiles: Integer;
    HypoTax: Integer;
    HypoLux: Integer;
    Working: Integer;
    Happy: Integer;
    FoodRep: Integer;
    ProdRep: Integer;
    Trade: Integer;
    PollRep: Integer;
    Corruption: Integer;
    Tax: Integer;
    Lux: Integer;
    Science: Integer;
    Support: Integer;
    Eaten: Integer;
    ProdCost: Integer;
    Storage: Integer;
    Deployed: Integer;
  end;

  TCityReportNew = record
    HypoTiles: Integer;
    // tiles that should be considered as exploited (for the current adjustment, set this to -1 or to TCity.Tiles of the city)
    HypoTaxRate: Integer;
    HypoLuxuryRate: Integer;
    // tax and luxury rate that should be assumed (for current rates, set this to -1 or to RO.TaxRate resp. RO.LuxRate)
    Morale: Integer;
    FoodSupport: Integer;
    MaterialSupport: Integer;
    // food and material taken for unit support
    ProjectCost: Integer; // material cost of current project
    Storage: Integer; // size of food storage
    Deployed: Integer; // number of units causing unrest (unrest=2*deployed)
    CollectedControl: Integer;
    CollectedFood: Integer;
    CollectedMaterial: Integer;
    CollectedTrade: Integer;
    // raw control, food, material and trade as collected by the citizens
    Working: Integer; // number of exploited tiles including city tile
    FoodSurplus: Integer;
    Production: Integer;
    AddPollution: Integer;
    // food surplus, production gain and pollution after all effects
    Corruption: Integer;
    Tax: Integer;
    Science: Integer;
    Luxury: Integer;
    // corruption, tax, science and wealth after all effects
    HappinessBalance: Integer;
    // = (Morale+Wealth+Control) - (Size+Unrest), value < 0 means disorder
  end;

  TCityTileAdviceData = record
    ResourceWeights: Cardinal;
    Tiles: Cardinal;
    CityReport: TCityReport;
  end;

  TGetCityData = record
    Owner: Integer;
    C: TCity;
  end;

  TCityAreaInfo = record
    Available: array [0 .. 26] of Integer;
  end;

  TUnitReport = record
    FoodSupport: Integer;
    ProdSupport: Integer;
    ReportFlags: Integer;
  end;

  TJobProgressData = array [0 .. nJob - 1] of record
    Required, Done,
    NextTurnPlus: Integer;
  end;

  TBattleForecast = record
    pAtt: Integer;
    mixAtt: Integer;
    HealthAtt: Integer;
    ExpAtt: Integer;
    FlagsAtt: Integer;
    Movement: Integer;
    EndHealthDef: Integer;
    EndHealthAtt: Integer;
  end;

  TBattleForecastEx = record
    pAtt: Integer;
    mixAtt: Integer;
    HealthAtt: Integer;
    ExpAtt: Integer;
    FlagsAtt: Integer;
    Movement: Integer;
    EndHealthDef: Integer;
    EndHealthAtt: Integer; // must be same as in TBattleForecast
    AStr: Integer;
    DStr: Integer;
    ABaseDamage: Integer;
    DBaseDamage: Integer;
  end;

  TShowMove = record
    Owner: Integer;
    Health: Integer;
    mix: Integer;
    emix: Integer;
    Flags: Integer;
    FromLoc: Integer;
    dx: Integer;
    dy: Integer;
    EndHealth: Integer;
    EndHealthDef: Integer;
    Fuel: Integer;
    Exp: Integer;
    Load: Integer;
  end;

  TShowShipChange = record
    Reason: Integer;
    Ship1Owner: Integer;
    Ship2Owner: Integer;
    Ship1Change: array [0 .. nShipPart - 1] of Integer;
    Ship2Change: array [0 .. nShipPart - 1] of Integer;
  end;

  TOffer = record
    nDeliver: Integer;
    nCost: Integer;
    Price: array [0 .. 11] of Cardinal;
  end;

  TChart = array [0 .. INFIN] of Integer;
  TEditTileData = record
    Loc: Integer;
    NewTile: Integer;
  end;
  TCreateUnitData = record
    Loc: Integer;
    P: Integer;
    mix: Integer;
  end;

  TTileList = array [0 .. INFIN] of Cardinal;
  PTileList = ^TTileList;
  TTileObservedLastList = array [0 .. INFIN] of SmallInt;
  TOwnerList = array [0 .. INFIN] of ShortInt;
  TByteList = array [0 .. INFIN] of Byte;
  TIntList = array [0 .. INFIN] of Integer;
  TCityList = array [0 .. INFIN] of TCity;
  TUnList = array [0 .. INFIN] of TUn;
  TModelList = array [0 .. INFIN] of TModel;
  TEnemyUnList = array [0 .. INFIN] of TUnitInfo;
  TEnemyCityList = array [0 .. INFIN] of TCityInfo;
  TEnemyModelList = array [0 .. INFIN] of TModelInfo;
  TBattleList = array [0 .. INFIN] of TBattle;

  TPlayerContext = record
    Data: Pointer;
    Map: ^TTileList;
    { the playground, a list of tiles with index = location, see tile flags }
    MapObservedLast: ^TTileObservedLastList;
    // turn in which the tile was observed last, index = location
    Territory: ^TOwnerList; // nation to which's territory a tile belongs, -1 indicates none
    Un: ^TUnList; { units }
    City: ^TCityList; { cities }
    Model: ^TModelList; { unit models }
    EnemyUn: ^TEnemyUnList; // known units of enemy players
    EnemyCity: ^TEnemyCityList; // known cities of enemy players
    EnemyModel: ^TEnemyModelList; // known unit models of enemy players
    EnemyReport: array [0 .. nPl - 1] of ^TEnemyReport;

    TestFlags: Integer; // options turned on in the "Manipulation" menu
    Turn: Integer; // current turn
    Alive: Integer; { bitset of IDs of players still alive, flag 1 shl p for player p }
    Happened: Integer; // flags indicate what happened within the last turnaround
    AnarchyStart: Integer; // start turn of anarchy, <0 if not in anarchy
    Credibility: Integer; // own credibility
    MaxCredibility: Integer; // maximum credibility still to achieve
    nUn: Integer; { number of units }
    nCity: Integer; { number of cities }
    nModel: Integer; { number of developed unit models }
    nEnemyUn: Integer;
    nEnemyCity: Integer;
    nEnemyModel: Integer;
    Government: Integer; { gAnarchy..gDemocracy }
    Money: Integer;
    TaxRate: Integer;
    LuxRate: Integer;
    Research: Integer;
    { collected research points for currently researched tech }
    ResearchTech: Integer; // currently researched tech
    DevModel: TModel; { unit model currently under development }
    Tech: array [0 .. (nAdv + 3) div 4 * 4 - 1] of ShortInt; { tech status indicator }
    Attitude: array [0 .. nPl - 1] of Integer; // attitude to other nations
    Treaty: array [0 .. nPl - 1] of Integer; // treaty with other nations
    EvaStart: array [0 .. nPl - 1] of Integer; // peace treaty: start of evacuation period
    Tribute: array [0 .. nPl - 1] of Integer; // no longer in use
    TributePaid: array [0 .. nPl - 1] of Integer; // no longer in use
    Wonder: array [0 .. nWonder - 1] of TWonderInfo;
    Ship: array [0 .. nPl - 1] of TShipInfo;
    NatBuilt: array [nWonder .. (nImp + 3) div 4 * 4 - 1] of ShortInt;
    nBattleHistory: Integer;
    BattleHistory: ^TBattleList; // complete list of all my battles in the whole game
    BorderHelper: ^TByteList;
    LastCancelTreaty: array [0 .. nPl - 1] of Integer; // turn of last treaty cancel
    OracleIncome: Integer;
    DefaultDebugMap: ^TIntList;
    Filler: array [0 .. 879] of Byte;
  end;

  TInitModuleData = record
    Server: TServerCall;
    DataVersion: Integer;
    DataSize: Integer;
    Flags: Integer;
  end;
  PInitModuleData = ^TInitModuleData;

  TNewGameData = record
    lx: Integer; // Map width
    ly: Integer; // Map height
    LandMass: Integer;
    MaxTurn: Integer;
    Difficulty: array [0 .. nPl - 1] of Integer;
    { difficulty levels of the players, if it's 0 this player is the supervisor,
      -1 for unused slots }
    RO: array [0 .. nPl - 1] of ^TPlayerContext;
    AssemblyPath: array [0 .. 255] of Char;
    SuperVisorRO: array [0 .. nPl - 1] of ^TPlayerContext;
  end;

  TNewGameExData = record
    lx: Integer;
    ly: Integer;
    LandMass: Integer;
    MaxTurn: Integer;
    RND: Integer;
    Difficulty: array [0 .. nPl - 1] of Integer;
    { difficulty levels of the players, if it's 0 this player is the supervisor,
      -1 for unused slots }
    Controlled: Integer;
  end;

  TShowNegoData = record
    pSender: Integer;
    pTarget: Integer;
    Action: Integer;
    Offer: TOffer;
  end;

const
  { predefined unit models: }
  nSpecialModel = 9;
  SpecialModel: array [0 .. nSpecialModel - 1] of TModel = ((Status: 0;
    SavedStatus: 0; ID: 0; IntroTurn: 0; Built: 0; Lost: 0; Kind: mkSettler;
    Domain: dGround; Attack: 0; Defense: 10; Speed: 150; Cost: 40; MStrength: 0;
    MTrans: 0; MCost: 0; Weight: 0; MaxWeight: 0; Upgrades: 0; Flags: 0;
    Cap: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0)), { Settlers }
    (Status: 0; SavedStatus: 0; ID: 0; IntroTurn: 0; Built: 0; Lost: 0;
    Kind: mkSettler; Domain: dGround; Attack: 0; Defense: 20; Speed: 300;
    Cost: 40; MStrength: 0; MTrans: 0; MCost: 0; Weight: 0; MaxWeight: 0;
    Upgrades: 0; Flags: 0; Cap: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Engineers }
    (Status: 0; SavedStatus: 0; ID: 0; IntroTurn: 0; Built: 0; Lost: 0;
    Kind: mkSelfDeveloped; Domain: dGround; Attack: 6; Defense: 6; Speed: 150;
    Cost: 10; MStrength: 0; MTrans: 0; MCost: 0; Weight: 0; MaxWeight: 0;
    Upgrades: 0; Flags: 0; Cap: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Militia }
    (Status: 0; SavedStatus: 0; ID: 0; IntroTurn: 0; Built: 0; Lost: 0;
    Kind: mkSpecial_TownGuard; Domain: dGround; Attack: 4; Defense: 6;
    Speed: 150; Cost: 20; MStrength: 0; MTrans: 0; MCost: 0; Weight: 0;
    MaxWeight: 0; Upgrades: 0; Flags: 0;
    Cap: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0)), { Town Guard }
    (Status: 0; SavedStatus: 0; ID: 0; IntroTurn: 0; Built: 0; Lost: 0;
    Kind: mkDiplomat; Domain: dGround; Attack: 12; Defense: 12; Speed: 250;
    Cost: 20; MStrength: 0; MTrans: 0; MCost: 0; Weight: 0; MaxWeight: 0;
    Upgrades: 0; Flags: 0; Cap: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Special Commando }
    (Status: 0; SavedStatus: 0; ID: 0; IntroTurn: 0; Built: 0; Lost: 0;
    Kind: mkCaravan; Domain: dGround; Attack: 0; Defense: 6; Speed: 150;
    Cost: 60; MStrength: 0; MTrans: 0; MCost: 0; Weight: 0; MaxWeight: 0;
    Upgrades: 0; Flags: 0; Cap: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Freight }
    (Status: 0; SavedStatus: 0; ID: 0; IntroTurn: 0; Built: 0; Lost: 0;
    Kind: mkSpecial_Boat; Domain: dSea; Attack: 0; Defense: 3; Speed: 250;
    Cost: 20; MStrength: 0; MTrans: 1; MCost: 0; Weight: 0; MaxWeight: 0;
    Upgrades: 0; Flags: 0; Cap: (0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Longboat }
    (Status: 0; SavedStatus: 0; ID: 0; IntroTurn: 0; Built: 0; Lost: 0;
    Kind: mkSlaves; Domain: dGround; Attack: 0; Defense: 15; Speed: 150;
    Cost: 40; MStrength: 0; MTrans: 0; MCost: 0; Weight: 0; MaxWeight: 0;
    Upgrades: 0; Flags: 0; Cap: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), { Slaves }
    { (Status:0;SavedStatus:0;ID:0;IntroTurn:0;Built:0;Lost:0;
      Kind:mkSpecial_Carriage;Domain:dGround;Attack:50;Defense:30;Speed:250;Cost:50;
      MStrength:0;MTrans:0;MCost:0;Weight:0;MaxWeight:0;Upgrades:0;Flags:0;
      Cap:(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), }
    { (Status:0;SavedStatus:0;ID:0;IntroTurn:0;Built:0;Lost:0;
      Kind:mkSpecial_SubCabin;Domain:dSea;Attack:16;Defense:1;Speed:350;Cost:40;
      MStrength:0;MTrans:0;MCost:0;Weight:0;MaxWeight:0;Upgrades:0;Flags:0;
      Cap:(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), }
    (Status: 0; SavedStatus: 0; ID: 0; IntroTurn: 0; Built: 0; Lost: 0;
    Kind: mkSpecial_Glider; Domain: dAir; Attack: 6; Defense: 6; Speed: 450;
    Cost: 30; MStrength: 0; MTrans: 0; MCost: 0; Weight: 0; MaxWeight: 0;
    Upgrades: 0; Flags: 0; Cap: (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)));

  SpecialModelPreq: array [0 .. nSpecialModel - 1] of Integer = (preNone,
    adExplosives, preNone, preNone, (* adWri, *) adIntelligence, adTrade,
    (* adTheCorporation,adHorsebackRiding,adAutomobile,adNavigation,
      adCombustionEngine, *) adMapMaking, preBuilder,
    { preLeo,preLighthouse, } preLeo);

var
  DelphiRandSeed: Integer;

procedure MakeUnitInfo(P: Integer; const U: TUn; var ui: TUnitInfo);
procedure MakeModelInfo(P, mix: Integer; const M: TModel; var mi: TModelInfo);
function IsSameModel(const mi1, mi2: TModelInfo): Boolean;
function SpecialTile(Loc, TerrType, lx: Integer): Integer;
function DelphiRandom(const pi_Max: Integer): Integer; overload;
function DelphiRandom: Extended; overload;
procedure DelphiRandomize;
function GetCommandDataSize(Command: TCommand): Integer;


implementation

procedure MakeUnitInfo(P: Integer; const U: TUn; var ui: TUnitInfo);
begin
  ui.Owner := P;
  ui.Loc := U.Loc;
  ui.Health := U.Health;
  ui.Fuel := U.Fuel;
  ui.Job := U.Job;
  ui.Exp := U.Exp;
  ui.Load := U.TroopLoad + U.AirLoad;
  ui.mix := U.mix;
  ui.Flags := U.Flags;
end;

procedure MakeModelInfo(P, mix: Integer; const M: TModel; var mi: TModelInfo);
var
  I: Integer;
begin
  mi.Owner := P;
  mi.mix := mix;
  mi.ID := M.ID;
  mi.Domain := M.Domain;
  if M.Kind = mkEnemyDeveloped then
    mi.Kind := mkSelfDeveloped // important for IsSameModel()
  else
    mi.Kind := M.Kind;
  mi.Attack := M.Attack;
  mi.Defense := M.Defense;
  mi.Speed := M.Speed;
  mi.Cost := M.Cost;
  if mi.Domain = dAir then
  begin
    mi.TTrans := M.Cap[mcAirTrans] * M.MTrans;
    mi.ATrans_Fuel := M.Cap[mcFuel];
  end
  else
  begin
    mi.TTrans := M.Cap[mcSeaTrans] * M.MTrans;
    mi.ATrans_Fuel := M.Cap[mcCarrier] * M.MTrans;
  end;
  mi.Bombs := M.Cap[mcBombs] * M.MStrength * 2;
  mi.Cap := 0;
  for I := mcFirstNonCap to nFeature - 1 do
    if M.Cap[I] > 0 then
      mi.Cap := mi.Cap or (1 shl (I - mcFirstNonCap));
  mi.MaxUpgrade := 0;
  for I := 1 to nUpgrade - 1 do
    if M.Upgrades and (1 shl I) <> 0 then
      mi.MaxUpgrade := I;
  mi.Weight := M.Weight;
  mi.Lost := 0;
end;

function IsSameModel(const mi1, mi2: TModelInfo): Boolean;
type
  TModelInfo_Compare = array [0 .. 5] of Cardinal;
var
  Compare1, Compare2: ^TModelInfo_Compare;
begin
  Compare1 := @mi1;
  Compare2 := @mi2;
  Result := (Compare1[1] and $FFFF0000 = Compare2[1] and $FFFF0000) and
    (Compare1[2] = Compare2[2]) and (Compare1[3] = Compare2[3]) and
    (Compare1[4] = Compare2[4]) and (Compare1[5] = Compare2[5])
end;

function SpecialTile(Loc, TerrType, lx: Integer): Integer;
var
  X, Y, qx, qy, A: Integer;
begin
  if TerrType = fOcean then
    Result := 0
  else
  begin
    Y := Loc div lx;
    X := Loc - Y * lx;
    if TerrType = fGrass then { formula for productive grassland }
      if Odd((lymax + X - Y shr 1) shr 1 + X + (Y + 1) shr 1) then
        Result := 1
      else
        Result := 0
    else { formula for special resources }
    begin
      A := 4 * X - Y + 9980;
      qx := A div 10;
      if (qx * 10 = A) and (qx and 3 <> 0) then
      begin
        qy := (Y + X) div 5;
        if qy and 3 <> qx shr 2 and 1 * 2 then
          if (TerrType = fArctic) or (TerrType = fSwamp) then
            Result := 1
          else if TerrType = fShore then
          begin
            if (qx + qy) and 1 = 0 then
              if qx and 3 = 2 then
                Result := 2
              else
                Result := 1
            else
              Result := 0
          end
          else
            Result := (qx + qy) and 1 + 1
        else
          Result := 0;
      end
      else
        Result := 0;
    end;
  end;
end;

procedure InitUnit;
begin
  Assert(SizeOf(TPlayerContext) = 1936 + 28 * SizeOf(Pointer));
  Assert(SizeOf(TModel) - 2 * SizeOf(LongInt) - 4 * SizeOf(Word)
  = sIntSetDevModel and $F * 4);
end;

function DelphiRandom(const pi_Max: Integer): Integer;
var
  Temp: LongInt;
begin
  Temp := LongInt(Int64(134775813) * Int64(DelphiRandSeed) + 1);
  DelphiRandSeed := LongInt(Temp);
  Result := (UInt64(Cardinal(pi_Max)) * UInt64(Cardinal(Temp))) shr 32;
end;

function DelphiRandom: Extended; overload;
begin
  Result := DelphiRandom(High(LongInt)) / High(LongInt);
end;

procedure DelphiRandomize;
begin
  Randomize;
  DelphiRandSeed := RandSeed;
end;

function GetCommandDataSize(Command: TCommand): Integer;
begin
  case Command of
    cmInitModule: Result := SizeOf(TInitModuleData);
    cmGetReady: Result := 0;
    cmTurn: Result := 0;
    cmShowTurnChange: Result := SizeOf(Integer);
    cmShowNego: Result := SizeOf(TShowNegoData);
    cmNewGame, cmLoadGame, cmMovie, cmNewMap: Result := SizeOf(TNewGameData);
    cmShowShipChange: Result := SizeOf(TShowShipChange);
    cmShowGreatLibTech: Result := SizeOf(Integer);
    cmShowCityChanged: Result := SizeOf(Integer);
    cmShowPeaceViolation: Result := SizeOf(Integer);
    cmShowMoving: Result := SizeOf(TShowMove);
    cmShowUnitChanged: Result := SizeOf(Integer);
    cmShowMissionResult: Result := SizeOf(Cardinal);
    cmShowAfterMove: Result := SizeOf(Integer);
    cmShowAfterAttack: Result := SizeOf(Integer);
    cmShowSupportAllianceAgainst: Result := SizeOf(Integer);
    cmShowCancelTreatyByAlliance: Result := SizeOf(Integer);
    cmShowEndContact: Result := 0;
    cmShowGame: Result := 0;
    //sIntCancelTreaty: Result := SizeOf(Integer);
    else begin
      Result := 0;
    end;
  end;
end;


initialization

InitUnit;

end.
