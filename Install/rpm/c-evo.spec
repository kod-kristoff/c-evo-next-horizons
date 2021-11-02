Name:           c-evo
Version:        1.4.0
Release:        1%{?dist}
Summary:        Empire building game

Group:          Development/Languages
License:        Public Domain
URL:            https://app.zdechov.net/c-evo
Source0:        c-evo.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Requires:       sox

#BuildRequires:  lazarus 

%description
Turn-based empire building game inspired by Civilization.

%global debug_package %{nil}

%prep
%setup -q -c $(RPM_NAME)-$(RPM_VERSION)


%build
(cd AI/StdAI ; lazbuild --build-mode=Release StdAI.lpi)
mv AI/StdAI/libstdai.so AI/StdAI/libstdai-%{_target_cpu}.so
sed -i 's/libstdai-i386.so/libstdai-%{_target_cpu}.so/g' AI/StdAI/StdAI.ai.txt
sed -i 's/libstdai-amd64.so/libstdai-%{_target_cpu}.so/g' AI/StdAI/StdAI.ai.txt
lazbuild --build-mode=Release Integrated.lpi

%install
rm -rf $RPM_BUILD_ROOT
install -d -m 755 $RPM_BUILD_ROOT/usr/share/c-evo
install -s -m 755 c-evo $RPM_BUILD_ROOT/usr/share/c-evo
install -d -m 755 $RPM_BUILD_ROOT/usr/bin
install -m 755 Install/deb/debian/c-evo.sh $RPM_BUILD_ROOT/usr/bin/c-evo
install -d -m 755 $RPM_BUILD_ROOT/usr/share/applications
install -m 755 Install/deb/c-evo.desktop $RPM_BUILD_ROOT/usr/share/applications
install -d -m 755 $RPM_BUILD_ROOT/usr/share/pixmaps
install -m 644 Graphics/c-evo_64x64.png $RPM_BUILD_ROOT/usr/share/pixmaps/c-evo.png
install -d -m 755 $RPM_BUILD_ROOT/usr/share/c-evo/AI/StdAI
install -m 644 AI/StdAI/libstdai-%{_target_cpu}.so $RPM_BUILD_ROOT/usr/share/c-evo/AI/StdAI
install -m 644 AI/StdAI/StdAI.ai.txt $RPM_BUILD_ROOT/usr/share/c-evo/AI/StdAI
install -m 644 AI/StdAI/StdAI.png $RPM_BUILD_ROOT/usr/share/c-evo/AI/StdAI
install -d -m 755 $RPM_BUILD_ROOT/usr/share/c-evo/Graphics
install -D -m 644 Graphics/* $RPM_BUILD_ROOT/usr/share/c-evo/Graphics
install -d -m 755 $RPM_BUILD_ROOT/usr/share/c-evo/Help
install -D -m 644 Help/* $RPM_BUILD_ROOT/usr/share/c-evo/Help
install -d -m 755 $RPM_BUILD_ROOT/usr/share/c-evo/Sounds
install -D -m 644 Sounds/* $RPM_BUILD_ROOT/usr/share/c-evo/Sounds
install -d -m 755 $RPM_BUILD_ROOT/usr/share/c-evo/Tribes
install -D -m 644 Tribes/* $RPM_BUILD_ROOT/usr/share/c-evo/Tribes
install -d -m 755 $RPM_BUILD_ROOT/usr/share/c-evo/Maps
install -D -m 644 Maps/* $RPM_BUILD_ROOT/usr/share/c-evo/Maps
install -d -m 755 $RPM_BUILD_ROOT/usr/share/c-evo/Saved
install -D -m 644 Saved/* $RPM_BUILD_ROOT/usr/share/c-evo/Saved
install -d -m 755 $RPM_BUILD_ROOT/usr/share/c-evo/AI\ Template
cp -R AI\ Template $RPM_BUILD_ROOT/usr/share/c-evo
install -d -m 755 $RPM_BUILD_ROOT/usr/share/c-evo/Localization
cp -R Localization $RPM_BUILD_ROOT/usr/share/c-evo

install -m 644 Language.txt $RPM_BUILD_ROOT/usr/share/c-evo/
install -m 644 Language2.txt $RPM_BUILD_ROOT/usr/share/c-evo/
install -m 644 Fonts.txt $RPM_BUILD_ROOT/usr/share/c-evo/

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
/usr/bin/c-evo
/usr/share/applications/c-evo.desktop
/usr/share/c-evo/*
/usr/share/pixmaps/c-evo.png

%doc readme.txt


%changelog
