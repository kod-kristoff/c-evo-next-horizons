unit Global;

interface

const
  CevoExt = '.cevo';
  CevoMapExt = '.cevomap';
  CevoMapPictureExt = '.png';
  CevoTribeExt = '.tribe.txt';
  CevoHomepageShort = 'app.zdechov.net/c-evo';
  CevoHomepage = 'https://' + CevoHomepageShort;
  CevoContactShort = 'app.zdechov.net/c-evo#Contact';
  CevoContact = 'https://' + CevoContactShort;
  CevoContactBug = 'https://app.zdechov.net/c-evo/report/1';
  CevoNetworkPort = 41363;
  AppRegistryKey = '\SOFTWARE\C-evo';
  AITemplateManual = 'AI development manual';
  AITemplateFileName = 'AI Template' + DirectorySeparator + AITemplateManual + '.html';
  CevoVersionMajor = 1;
  CevoVersionMinor = 3;
  CevoVersionBugFix = 0;
  CevoVersion = ((CevoVersionMajor and $ff) shl 16) or
    ((CevoVersionMinor and $ff) shl 8) or
    ((CevoVersionBugFix and $ff) shl 0);


implementation

end.

