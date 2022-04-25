unit ULanguages;

interface

uses
  Classes, SysUtils, fgl;

type
  TLanguage = class
    Name: string;
    Code: string;
    Available: Boolean;
  end;

  { TLanguages }

  TLanguages = class(TFPGObjectList<TLanguage>)
    function SearchByCode(ACode: string): TLanguage;
    procedure AddNew(Code: string; Name: string);
    constructor Create(FreeObjects: Boolean = True);
  end;


resourcestring
  SLangAuto = 'Automatic';
  SLang_aa = 'Afar';
  SLang_ab = 'Abkhazian';
  SLang_ae = 'Avestan';
  SLang_af = 'Afrikaans';
  SLang_ak = 'Akan';
  SLang_am = 'Amharic';
  SLang_an = 'Aragonese';
  SLang_ar = 'Arabic';
  SLang_as = 'Assamese';
  SLang_av = 'Avaric';
  SLang_ay = 'Aymara';
  SLang_az = 'Azerbaijani';
  SLang_ba = 'Bashkir';
  SLang_be = 'Belarusian';
  SLang_bg = 'Bulgarian';
  SLang_bh = 'Bihari';
  SLang_bi = 'Bislama';
  SLang_bm = 'Bambara';
  SLang_bn = 'Bengali';
  SLang_bo = 'Tibetan';
  SLang_br = 'Breton';
  SLang_bs = 'Bosnian';
  SLang_ca = 'Catalan';
  SLang_ce = 'Chechen';
  SLang_ch = 'Chamorro';
  SLang_co = 'Corsican';
  SLang_cr = 'Cree';
  SLang_cs = 'Czech';
  SLang_cv = 'Chuvash';
  SLang_cy = 'Welsh';
  SLang_da = 'Danish';
  SLang_de = 'German';
  SLang_de_AT = 'Austrian German';
  SLang_de_CH = 'Swiss German';
  SLang_dv = 'Divehi';
  SLang_dz = 'Dzongkha';
  SLang_ee = 'Ewe';
  SLang_el = 'Greek';
  SLang_en = 'English';
  SLang_en_AU = 'Australian English';
  SLang_en_CA = 'Canadian English';
  SLang_en_GB = 'British English';
  SLang_en_US = 'American English';
  SLang_eo = 'Esperanto';
  SLang_es = 'Spanish';
  SLang_et = 'Estonian';
  SLang_eu = 'Basque';
  SLang_fa = 'Persian';
  SLang_ff = 'Fulah';
  SLang_fi = 'Finnish';
  SLang_fj = 'Fijian';
  SLang_fo = 'Faroese';
  SLang_fr = 'French';
  SLang_fr_BE = 'Walloon';
  SLang_fy = 'Frisian';
  SLang_ga = 'Irish';
  SLang_gd = 'Gaelic';
  SLang_gl = 'Gallegan';
  SLang_gn = 'Guarani';
  SLang_gu = 'Gujarati';
  SLang_gv = 'Manx';
  SLang_ha = 'Hausa';
  SLang_he = 'Hebrew';
  SLang_hi = 'Hindi';
  SLang_ho = 'Hiri Motu';
  SLang_hr = 'Croatian';
  SLang_ht = 'Haitian';
  SLang_hu = 'Hungarian';
  SLang_hy = 'Armenian';
  SLang_hz = 'Herero';
  SLang_ia = 'Interlingua';
  SLang_id = 'Indonesian';
  SLang_ie = 'Interlingue';
  SLang_ig = 'Igbo';
  SLang_ii = 'Sichuan Yi';
  SLang_ik = 'Inupiaq';
  SLang_io = 'Ido';
  SLang_is = 'Icelandic';
  SLang_it = 'Italian';
  SLang_iu = 'Inuktitut';
  SLang_ja = 'Japanese';
  SLang_jv = 'Javanese';
  SLang_ka = 'Georgian';
  SLang_kg = 'Kongo';
  SLang_ki = 'Kikuyu';
  SLang_kj = 'Kuanyama';
  SLang_kk = 'Kazakh';
  SLang_kl = 'Greenlandic';
  SLang_km = 'Khmer';
  SLang_kn = 'Kannada';
  SLang_ko = 'Korean';
  SLang_kr = 'Kanuri';
  SLang_ks = 'Kashmiri';
  SLang_ku = 'Kurdish';
  SLang_kw = 'Cornish';
  SLang_kv = 'Komi';
  SLang_ky = 'Kirghiz';
  SLang_la = 'Latin';
  SLang_lb = 'Luxembourgish';
  SLang_lg = 'Ganda';
  SLang_li = 'Limburgan';
  SLang_ln = 'Lingala';
  SLang_lo = 'Lao';
  SLang_lt = 'Lithuanian';
  SLang_lu = 'Luba-Katanga';
  SLang_lv = 'Latvian';
  SLang_mg = 'Malagasy';
  SLang_mh = 'Marshallese';
  SLang_mi = 'Maori';
  SLang_mk = 'Macedonian';
  SLang_ml = 'Malayalam';
  SLang_mn = 'Mongolian';
  SLang_mo = 'Moldavian';
  SLang_mr = 'Marathi';
  SLang_ms = 'Malay';
  SLang_mt = 'Maltese';
  SLang_my = 'Burmese';
  SLang_na = 'Nauru';
  SLang_nb = 'Norwegian Bokmaal';
  SLang_nd = 'Ndebele, North';
  SLang_ne = 'Nepali';
  SLang_ng = 'Ndonga';
  SLang_nl = 'Dutch';
  SLang_nl_BE = 'Flemish';
  SLang_nn = 'Norwegian Nynorsk';
  SLang_no = 'Norwegian';
  SLang_nr = 'Ndebele, South';
  SLang_nv = 'Navajo';
  SLang_ny = 'Chichewa';
  SLang_oc = 'Occitan';
  SLang_oj = 'Ojibwa';
  SLang_om = 'Oromo';
  SLang_or = 'Oriya';
  SLang_os = 'Ossetian';
  SLang_pa = 'Panjabi';
  SLang_pi = 'Pali';
  SLang_pl = 'Polish';
  SLang_ps = 'Pushto';
  SLang_pt = 'Portuguese';
  SLang_pt_BR = 'Brazilian Portuguese';
  SLang_qu = 'Quechua';
  SLang_rm = 'Raeto-Romance';
  SLang_rn = 'Rundi';
  SLang_ro = 'Romanian';
  SLang_ru = 'Russian';
  SLang_rw = 'Kinyarwanda';
  SLang_sa = 'Sanskrit';
  SLang_sc = 'Sardinian';
  SLang_sd = 'Sindhi';
  SLang_se = 'Northern Sami';
  SLang_sg = 'Sango';
  SLang_si = 'Sinhalese';
  SLang_sk = 'Slovak';
  SLang_sl = 'Slovenian';
  SLang_sm = 'Samoan';
  SLang_sn = 'Shona';
  SLang_so = 'Somali';
  SLang_sq = 'Albanian';
  SLang_sr = 'Serbian';
  SLang_ss = 'Swati';
  SLang_st = 'Sotho, Southern';
  SLang_su = 'Sundanese';
  SLang_sv = 'Swedish';
  SLang_sw = 'Swahili';
  SLang_ta = 'Tamil';
  SLang_te = 'Telugu';
  SLang_tg = 'Tajik';
  SLang_th = 'Thai';
  SLang_ti = 'Tigrinya';
  SLang_tk = 'Turkmen';
  SLang_tl = 'Tagalog';
  SLang_tn = 'Tswana';
  SLang_to = 'Tonga';
  SLang_tr = 'Turkish';
  SLang_ts = 'Tsonga';
  SLang_tt = 'Tatar';
  SLang_tw = 'Twi';
  SLang_ty = 'Tahitian';
  SLang_ug = 'Uighur';
  SLang_uk = 'Ukrainian';
  SLang_ur = 'Urdu';
  SLang_uz = 'Uzbek';
  SLang_ve = 'Venda';
  SLang_vi = 'Vietnamese';
  SLang_vo = 'Volapuk';
  SLang_wa = 'Walloon';
  SLang_wo = 'Wolof';
  SLang_xh = 'Xhosa';
  SLang_yi = 'Yiddish';
  SLang_yo = 'Yoruba';
  SLang_za = 'Zhuang';
  SLang_zh = 'Chinese';
  SLang_zu = 'Zulu';

implementation


{ TLanguages }

function TLanguages.SearchByCode(ACode: string): TLanguage;
var
  I: Integer;
begin
  I := 0;
  while (I < Count) and (TLanguage(Items[I]).Code < ACode) do Inc(I);
  if I < Count then Result := TLanguage(Items[I])
    else Result := nil;
end;

procedure TLanguages.AddNew(Code: string; Name: string);
var
  NewItem: TLanguage;
begin
  NewItem := TLanguage.Create;
  NewItem.Name := Name;
  NewItem.Code := Code;
  Add(NewItem);
end;

constructor TLanguages.Create(FreeObjects: Boolean);
begin
  inherited;
  AddNew('', SLangAuto);
  AddNew('aa', SLang_aa);
  AddNew('ab', SLang_ab);
  AddNew('ae', SLang_ae);
  AddNew('af', SLang_af);
  AddNew('ak', SLang_ak);
  AddNew('am', SLang_am);
  AddNew('an', SLang_an);
  AddNew('ar', SLang_ar);
  AddNew('as', SLang_as);
  AddNew('av', SLang_av);
  AddNew('ay', SLang_ay);
  AddNew('az', SLang_az);
  AddNew('ba', SLang_ba);
  AddNew('be', SLang_be);
  AddNew('bg', SLang_bg);
  AddNew('bh', SLang_bh);
  AddNew('bi', SLang_bi);
  AddNew('bm', SLang_bm);
  AddNew('bn', SLang_bn);
  AddNew('bo', SLang_bo);
  AddNew('br', SLang_br);
  AddNew('bs', SLang_bs);
  AddNew('ca', SLang_ca);
  AddNew('ce', SLang_ce);
  AddNew('ch', SLang_ch);
  AddNew('co', SLang_co);
  AddNew('cr', SLang_cr);
  AddNew('cs', SLang_cs);
  AddNew('cv', SLang_cv);
  AddNew('cy', SLang_cy);
  AddNew('da', SLang_da);
  AddNew('de', SLang_de);
  AddNew('de_AT', SLang_de_AT);
  AddNew('de_CH', SLang_de_CH);
  AddNew('dv', SLang_dv);
  AddNew('dz', SLang_dz);
  AddNew('ee', SLang_ee);
  AddNew('el', SLang_el);
  AddNew('en', SLang_en);
  AddNew('en_AU', SLang_en_AU);
  AddNew('en_CA', SLang_en_CA);
  AddNew('en_GB', SLang_en_GB);
  AddNew('en_US', SLang_en_US);
  AddNew('eo', SLang_eo);
  AddNew('es', SLang_es);
  AddNew('et', SLang_et);
  AddNew('eu', SLang_eu);
  AddNew('fa', SLang_fa);
  AddNew('ff', SLang_ff);
  AddNew('fi', SLang_fi);
  AddNew('fj', SLang_fj);
  AddNew('fo', SLang_fo);
  AddNew('fr', SLang_fr);
  AddNew('fr_BE', SLang_fr_BE);
  AddNew('fy', SLang_fy);
  AddNew('ga', SLang_ga);
  AddNew('gd', SLang_gd);
  AddNew('gl', SLang_gl);
  AddNew('gn', SLang_gn);
  AddNew('gu', SLang_gu);
  AddNew('gv', SLang_gv);
  AddNew('ha', SLang_ha);
  AddNew('he', SLang_he);
  AddNew('hi', SLang_hi);
  AddNew('ho', SLang_ho);
  AddNew('hr', SLang_hr);
  AddNew('ht', SLang_ht);
  AddNew('hu', SLang_hu);
  AddNew('hy', SLang_hy);
  AddNew('hz', SLang_hz);
  AddNew('ia', SLang_ia);
  AddNew('id', SLang_id);
  AddNew('ie', SLang_ie);
  AddNew('ig', SLang_ig);
  AddNew('ii', SLang_ii);
  AddNew('ik', SLang_ik);
  AddNew('io', SLang_io);
  AddNew('is', SLang_is);
  AddNew('it', SLang_it);
  AddNew('iu', SLang_iu);
  AddNew('ja', SLang_ja);
  AddNew('jv', SLang_jv);
  AddNew('ka', SLang_ka);
  AddNew('kg', SLang_kg);
  AddNew('ki', SLang_ki);
  AddNew('kj', SLang_kj);
  AddNew('kk', SLang_kk);
  AddNew('kl', SLang_kl);
  AddNew('km', SLang_km);
  AddNew('kn', SLang_kn);
  AddNew('ko', SLang_ko);
  AddNew('kr', SLang_kr);
  AddNew('ks', SLang_ks);
  AddNew('ku', SLang_ku);
  AddNew('kw', SLang_kw);
  AddNew('kv', SLang_kv);
  AddNew('ky', SLang_ky);
  AddNew('la', SLang_la);
  AddNew('lb', SLang_lb);
  AddNew('lg', SLang_lg);
  AddNew('li', SLang_li);
  AddNew('ln', SLang_ln);
  AddNew('lo', SLang_lo);
  AddNew('lt', SLang_lt);
  AddNew('lu', SLang_lu);
  AddNew('lv', SLang_lv);
  AddNew('mg', SLang_mg);
  AddNew('mh', SLang_mh);
  AddNew('mi', SLang_mi);
  AddNew('mk', SLang_mk);
  AddNew('ml', SLang_ml);
  AddNew('mn', SLang_mn);
  AddNew('mo', SLang_mo);
  AddNew('mr', SLang_mr);
  AddNew('ms', SLang_ms);
  AddNew('mt', SLang_mt);
  AddNew('my', SLang_my);
  AddNew('na', SLang_na);
  AddNew('nb', SLang_nb);
  AddNew('nd', SLang_nd);
  AddNew('ne', SLang_ne);
  AddNew('ng', SLang_ng);
  AddNew('nl', SLang_nl);
  AddNew('nl_BE', SLang_nl_BE);
  AddNew('nn', SLang_nn);
  AddNew('no', SLang_no);
  AddNew('nr', SLang_nr);
  AddNew('nv', SLang_nv);
  AddNew('ny', SLang_ny);
  AddNew('oc', SLang_oc);
  AddNew('oj', SLang_oj);
  AddNew('om', SLang_om);
  AddNew('or', SLang_or);
  AddNew('os', SLang_os);
  AddNew('pa', SLang_pa);
  AddNew('pi', SLang_pi);
  AddNew('pl', SLang_pl);
  AddNew('ps', SLang_ps);
  AddNew('pt', SLang_pt);
  AddNew('pt_BR', SLang_pt_BR);
  AddNew('qu', SLang_qu);
  AddNew('rm', SLang_rm);
  AddNew('rn', SLang_rn);
  AddNew('ro', SLang_ro);
  AddNew('ru', SLang_ru);
  AddNew('rw', SLang_rw);
  AddNew('sa', SLang_sa);
  AddNew('sc', SLang_sc);
  AddNew('sd', SLang_sd);
  AddNew('se', SLang_se);
  AddNew('sg', SLang_sg);
  AddNew('si', SLang_si);
  AddNew('sk', SLang_sk);
  AddNew('sl', SLang_sl);
  AddNew('sm', SLang_sm);
  AddNew('sn', SLang_sn);
  AddNew('so', SLang_so);
  AddNew('sq', SLang_sq);
  AddNew('sr', SLang_sr);
  AddNew('ss', SLang_ss);
  AddNew('st', SLang_st);
  AddNew('su', SLang_su);
  AddNew('sv', SLang_sv);
  AddNew('sw', SLang_sw);
  AddNew('ta', SLang_ta);
  AddNew('te', SLang_te);
  AddNew('tg', SLang_tg);
  AddNew('th', SLang_th);
  AddNew('ti', SLang_ti);
  AddNew('tk', SLang_tk);
  AddNew('tl', SLang_tl);
  AddNew('tn', SLang_tn);
  AddNew('to', SLang_to);
  AddNew('tr', SLang_tr);
  AddNew('ts', SLang_ts);
  AddNew('tt', SLang_tt);
  AddNew('tw', SLang_tw);
  AddNew('ty', SLang_ty);
  AddNew('ug', SLang_ug);
  AddNew('uk', SLang_uk);
  AddNew('ur', SLang_ur);
  AddNew('uz', SLang_uz);
  AddNew('ve', SLang_ve);
  AddNew('vi', SLang_vi);
  AddNew('vo', SLang_vo);
  AddNew('wa', SLang_wa);
  AddNew('wo', SLang_wo);
  AddNew('xh', SLang_xh);
  AddNew('yi', SLang_yi);
  AddNew('yo', SLang_yo);
  AddNew('za', SLang_za);
  AddNew('zh', SLang_zh);
  AddNew('zu', SLang_zu);
end;

end.

