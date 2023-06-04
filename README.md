# mitebiomechanics
This is the repository for data and codes used in mite biomechanics paper
Biomechanical adaptations mediate spatial niche partitioning between phoretic mite species on burying beetles



Syuan-Jyun Sun1,2,† ,*, Simon Chen1,†, Walter Federle1, and Rebecca M. Kilner1

1 Department of Zoology, University of Cambridge, Downing Street, Cambridge, CB2 3EJ, UK.
2 International Degree Program in Climate Change and Sustainable Development, National Taiwan University, Taipei 10617, Taiwan

†These authors contributed equally to this study.

*Corresponding author: Syuan-Jyun Sun
Email: sjs243@ntu.edu.tw

We explored the spatial niche partitioning between two phoretic mite, Poecilochirus carabi and Macrocheles nataliae, that coexist on the host burying beetle Nicrophorus vespilloides. We further provided evidence with biomechanic approaches to reveal the underlying adaptive values of attachment to beetle's cuticle surface. We first surveyed the natural densities of mites on field-collected beetles, and determined their preferred attachment sites on beetles. Next, we manipulated densities of both mites to investigate how intra- and inter-specific interactions influenced mite spatial distribution. Using electron microscopy, we quantified the cuticle surface of all beetle body parts and mite attachment devices. To further assess if mite preference for specific attachment sites is adaptive, we tested mite attachment to both beetle surface and artificial test substrates. Together, we showed that specialised mite attachment to different beetle's body parts mediate spatial niche partitioning among mite species on their carrier.

Explanations for variables:
Fieldanalysis: 
Date: date of trapping event
year: year of trapping event
site: G (Gamlingay Wood)/W (Waresley Wood)
id: individual id
Trap: specific sites of trapping
body: beetle body parts
Sex: sex of beetles
bodysize: pronotum width of beetles
totalarea: total surface area of a given beetle (mm^2)
area: surface area of a given body part of a given beetle
head: surface area of the head of a given beetle
thorax: surface area of the thorax of a given beetle
abdomen: surface area of the abdomen of a given beetle
pronotum: surface area of the pronotum of a given beetle
elytra: surface area of the elytra of a given beetle
presence: presence of P. carabi only (big), M. nataliae only (small), both (both), or none (none)
bighead: number of P. carabi on the head of a given beetle
bigthorax: number of P. carabi on the thorax of a given beetle
bigabdomen: number of P. carabi on the abdomen of a given beetle
bigpronotum: number of P. carabi on the pronotum of a given beetle
bigelytra: number of P. carabi on the elytra of a given beetle
bignum: total number of P. carabi on a given beetle
predbighead: predicted number of P. carabi on the head of a given beetle, when multiplying the total number of mites by the percentage of each body part
predbigthorax: predicted number of P. carabi on the thorax of a given beetle, when multiplying the total number of mites by the percentage of each body part	
predbigabdomen: predicted number of P. carabi on the abdomen of a given beetle, when multiplying the total number of mites by the percentage of each body part		
predbigpronotum: predicted number of P. carabi on the pronotum of a given beetle, when multiplying the total number of mites by the percentage of each body part		
predbigelytra: predicted number of P. carabi on the elytra of a given beetle, when multiplying the total number of mites by the percentage of each body part
newbigmiteintensity: density of P. carabi (local number divided by local surface area) on a given body part
oldbigintensity: total density of P. carabi (total number divided by total surface area) of a beetle
smallhead: number of M. nataliae on the head of a given beetle
smallthorax: number of M. nataliae on the thorax of a given beetle	
smallabdomen: number of M. nataliae on the abdomen of a given beetle	
smallpronotum: number of M. nataliae on the pronotum of a given beetle	
smallelytra: number of M. nataliae on the elytra of a given beetle	
smallnum: total number of M. nataliae on a given beetle	
newsmallmiteintensity: density of M. nataliae (local number divided by local surface area) on a given body part
oldsmallintensity: total density of M. nataliae (total number divided by total surface area) of a beetle

force:
miteid/id: individual id of mite	
mite: species of mite	
substrate: tested on substrate or on beetle	
tr: specific body parts that mites were tested	
angles: angles of pulling force	
force: strength of pulling force (mN)

hair quantification on abdomen:
id: beetle individual id
replicate: replicates of sampled area on a given beetle
num: number of hairs
type: type of hair (long/short)

labspacecompetition:
id: beetle individual id
tr: mite density treatments (eg l50s50, meaning 50 P. carabi + 50 M. nataliae)
body: specific body parts
mite: mite species
num: number of mites
newintensity: the density of mites (for P. carabi or M. nataliae) on each specific body part
bodysize: pronotum width of a beetle
sex: sex of a beetle
bignum: number of P. carabi placed 
bigintensity: the total density of P. carabi on a beetle
smallnum: number of M. nataliae placed
smallintensity: the total density of M. nataliae on a beetle
naturalsetting: mite density for natural setting comparison (1), i.e. P. carabi ranging from 1, 10, 50, whereas, M. nataliae ranging from 1, 3, 5.

longhairnum:
imageid: image id of each beetle
id: individual beetle id	
replicate: replicated sampling sites of each image	
num: number of hair	
body: beetle's body parts	
hairlength: hair type as long or short 	
hairdensity: T for comparing long hair density on thorax vs short hair density on abdomen

mite reproduction:
pair: pairing id 	
wt: carcass mass (g)	
bl: block of experiment	
male: specific family that the male originated
female: specific family that the female originated	
mitetr: mite species
mitenum: number of mite offspring

padarea:
videoid: video id
mite: mite species
miteid: id of individual mite
stepid: step id of each individual mite
area: pad contact area (um^2)
