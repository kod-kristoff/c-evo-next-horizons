unit Names;

interface

uses
  Protocol;

const

  Name_Advance: array [0 .. nAdv - 1] of string = ('Advanced Flight',
    'Amphibious Warfare', 'Astronomy', 'Atomic Theory', 'Automobile',
    'Ballistics', 'Banking', 'Bridge Building', 'Bronze Working',
    'Ceremonial Burial', 'Chemistry', 'Chivalry', 'Composites', 'Code of Laws',
    'Combined Arms', 'Combustion Engine', 'Communism', 'Computers',
    'Conscription', 'Construction', 'The Corporation', 'Space Flight',
    'Currency', 'Democracy', 'Economics', 'Electricity', 'Electronics',
    'Engineering', 'Environmentalism', 'The Wheel', 'Explosives', 'Flight',
    'Intelligence', 'Gunpowder', 'Horseback Riding', 'Impulse Drive',
    'Industrialization', 'Intelligent Arms', 'Invention', 'Iron Working',
    'The Laser', 'Nuclear Power', 'Literature', 'Internet', 'Magnetism',
    'Map Making', 'Masonry', 'Mass Production', 'Mathematics', 'Medicine',
    'Metallurgy', 'Miniaturization', 'Mobile Warfare', 'Monarchy', 'Mysticism',
    'Navigation', 'Nuclear Fission', 'Philosophy', 'Physics', 'Plastics',
    'Poetry', 'Pottery', 'Radio Communication', 'Recycling', 'Refrigeration',
    'Monotheism', 'The Republic', 'Robotics', 'Rocketry', 'Railroad',
    'Sanitation', 'Science', 'Writing', 'Seafaring',
    'Self-Contained Environment', 'Stealth', 'Steam Engine', 'Steel',
    'Synthetic Food', 'Tactics', 'Theology', 'Theory of Gravity', 'Trade',
    'Transstellar Colonization', 'University', 'Advanced Rocketry',
    'Warrior Code', 'Alphabet', 'Polytheism', 'Refining',
    'Computing Technology', 'Nano Technology', 'Material Technology',
    'Artificial Intelligence');

  Name_Improvement: array [0 .. nImp - 1] of string = ('The Pyramids',
    'The Temple of Zeus', 'The Hanging Gardens', 'The Colossus',
    'The Lighthouse', 'The Great Library', 'The Oracle',
    'Sun Tzu''s War Academy', 'Leonardo''s Workshop', 'Magellan''s Expedition',
    'Michelangelo''s Chapel', '*', 'Newton''s College', 'Bach''s Cathedral',
    '*', 'The Statue of Liberty', 'The Eiffel Tower', 'The Hoover Dam',
    'The Shinkansen Express', 'The Manhattan Project', 'MIR Space Station', '*',
    '*', '*', '*', '*', '*', '*', 'Trade Goods', 'Barracks', 'Granary',
    'Temple', 'Marketplace', 'Library', 'Courthouse', 'City Walls', 'Aqueduct',
    'Bank', 'Cathedral', 'University', 'Harbor', 'Theater', 'Factory',
    'Manufacturing Plant', 'Recycling Center', 'Power Station',
    'Hydroelectric Dam', 'Nuclear Plant', 'Offshore Platform', 'Town Hall',
    'Sewer System', 'Supermarket', 'Superhighways', 'Research Lab',
    'SAM Missile Battery', 'Coastal Fortress', 'Airport', 'Dockyard', 'Palace',
    'Great Wall', 'Colosseum', 'Observatory', 'Military Academy',
    'Command Bunker', 'Algae Plant', 'Stock Exchange', 'Space Port',
    'Colony Ship Component', 'Power Module', 'Habitation Module');

  Name_Feature: array [0 .. nFeature - 1] of string = ('Weapons', 'Armor',
    'Mobility', 'Sea Transport', 'Carrier', 'Turbines', 'Bombs', 'Fuel',
    'Air Transport', 'Navigation', 'Radar / Sonar', 'Submarine', 'Artillery',
    'Alpine', 'Supply Ship', 'Overweight', 'Air Defence', 'Spy Plane',
    'Steam Power', 'Nuclear Power', 'Jet Engines', 'Stealth', 'Fanatic',
    'First Strike', 'Power of Will', 'Academy Training', 'Line Production');

  Name_TerrainType: array [0 .. 11] of string = ('Ocean', 'Coast', 'Grassland',
    'Desert', 'Prairie', 'Tundra', 'Arctic', 'Swamp', '*', 'Forest', 'Hills',
    'Mountains');

  Name_Government: array [0 .. nGov - 1] of string = ('Anarchy', 'Despotism',
    'Monarchy', 'Republic', 'Fundamentalism', 'Communism', 'Democracy',
    'Future Society');

implementation

end.
