Kodovani: UTF-8

				======================
				Čeština do C-evo 1.2.0
				======================


1. Hey, I don't understand this language!
-----------------------------------------
Then you don't need this package. It only contains czech localization of C-evo.
Use the original install program that includes the english version.
Go to page: http://c-evo.org


2. Co to je C-evo?
------------------
Hra ve stylu Civilization, kdy budujete vlastní říši a bojujete nebo diplomaticky
vyjednáváte s ostatními národy. Cílem hry je vybudovat mezihvězdnou kolonizační loď.
Jedná se o hru, kterou lze zdarma stáhnout z následujících stránek:
http://c-evo.org


3. Obsah tohoto balíku
----------------------
Všechny texty jsou přeloženy do češtiny. Včetně dvou obrázků v příručce.
Dále byla přeložena jména a jednotky národů, které jsou standardně dodávány se hrou.
Jako bonus byl přidán nový národ Čechů (o něm více na konci tohoto souboru).


4. Požadavky
------------
Tento balík lze bez problémů provozovat s C-evo verze 1.2.0.


5. Postup instalace češtiny do C-evo
------------------------------------
Máte dvě možnosti jak nainstalovat češtinu do hry C-evo:

  a) On-line instalace (doporučeno)
     Spusťte hru a v hlavním menu klikněte na záložku s logem C-evo. Spusťte nástroj
     "Configurator". V nabídce "Language" vyberte "Czech" a stiskněte tlačítko OK.
     Čeština bude automaticky stažena ze serveru C-evo a nainstalována do správné složky.

  b) Off-line instalace
     Pokud nemůžete použít on-line instalaci, spusťte soubor "offline_install.bat", který
     je součástí tohoto balíčku. Instalátor zajistí nakopírování souborů do správné složky.


6. Postup odinstalování češtiny
-------------------------------
Pokud jste použili on-line instalaci češtiny, silně doporučuji provést její odinstalaci
také on-line způsobem. Spusťte C-evo, spusťte "Konfigurátor" a v nabídce "Language"
vyberte jiný jazyk. Po stisknutí tlačítka OK dojde k odstranění češtiny.

Pokud jste použili off-line instalaci češtiny, spusťte off-line odinstalátor
"offline_uninstall.bat", který najdete rovněž v tomto balíčku.


7. Možné problémy
-----------------
Přeložené národy jsou navrženy speciálně tak, aby co nejlépe "pasovaly" do českých textů
ve hře. Tím je myšleno především správné skloňování.
Pokud do počeštěného C-evo přidáte další národ a začnete s ním hrát, pravděpodobně
se setkáte s tím, že některé věty nebudou dávat smysl. Vámi přidaný národ je potřeba
upravit, aby byl kompatibilní s naší češtinou.
Editujte soubor "<jméno_národa>.tribe.txt", patrně v něm najdete tyto řádky (pouze
s jiným jménem národa):

#n the Americans
#s America
#a American

Tyto řádky odstraňte a místo nich doplňte tyto řádky (se jménem daného národa):

#a Americký
#b Americká
#c Americké
#d Americkou
#e Amerického
#f Američané
#g Američany
#h Američanů
#i Američanech

Nezapomeňte jméno národa správně skloňovat. Po rozehrání hry bude již vše v pořádku.


8. Autoři
---------
Autor lokalizace textů a příručky:
 "tyllanthor" <tyl.lanthor@gmail.com> (dříve používající přezdívku "LFK")

Autor lokalizace národů:
 "idk0279"

Připomínky nebo informace o nalezených nedostatcích v překladu či překlepech v textu
zasílejte prosím na výše uvedenou e-mailovou adresu.


9. Poznámky
-----------
Český národ, který je přítomen v tomto balíčku, je oproti češtinám do starších verzí
C-evo "ořezaný"! Byl odstraněn český státní znak a pro obrázky českých jednotek se
používá výchozí grafika v C-evo.
Tuto změnu bylo potřeba provést, aby hra fungovala korektně v případě provedení on-line
instalace češtiny do C-evo.
Původní "neořezaný" český národ bude však možné stáhnout jako samostatný balíček
a doinstalovat do C-evo. Balíček bude v blízké budoucnosti zveřejněn na těchto stránkách:
http://cevocz.ic.cz
