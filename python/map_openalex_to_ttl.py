import pandas as pd
import json
import re
from rdflib import Namespace, URIRef, Graph, Literal
from rdflib.namespace import RDF
import gzip
import shutil

skos = Namespace("http://www.w3.org/2004/02/skos/core#")
oasis = Namespace("http://oasis.senckenberg.de/ontology/")
foaf = Namespace("http://xmlns.com/foaf/0.1/")
gndo = Namespace("http://d-nb.info/standards/elementset/gnd#")
owl = Namespace("http://www.w3.org/2002/07/owl#")
edm = Namespace("http://www.europeana.eu/schemas/edm/")
dc = Namespace ("http://purl.org/dc/elements/1.1/")
dcterms = Namespace ("http://purl.org/dc/terms/")
rdfs = Namespace ("http://www.w3.org/2000/01/rdf-schema#")
xsd = Namespace ("http://www.w3.org/2001/XMLSchema#")

graph = Graph()

graph.bind('skos', skos)
graph.bind ('foaf' , foaf)
graph.bind ('oasis' , oasis)
graph.bind('gndo',gndo)
graph.bind ('owl' , owl)
graph.bind('edm',edm)
graph.bind('dc',dc)
graph.bind('dcterms',dcterms)
graph.bind('rdfs',rdfs)
graph.bind("xsd", xsd)


# chunking
counter = 0
first_write = True

p = pd.read_parquet('output/matched_reviewed_refs.parquet')


oa_id_list=[]

for i in range(len(p)):


   oa_id_list.append(p.iloc[i]["OpenAlex_ID_short"])

with open("output/expanded_works.json", "r", encoding="utf-8") as f:

    data = json.load(f)

    n = pd.DataFrame(data["nodes"])

    e = pd.DataFrame(data["edges"])


for a in range(len(p)):

    oasisid = p.iloc[a]["OpenAlex_ID_short"]

    nameoasis = p.iloc[a]["Oases"]

    nameoasis = nameoasis.replace(' ','')

    oasisuri = 'http://oasis.senckenberg.de/ontology/Oasis_' + nameoasis

    graph.add((URIRef(oasisuri), RDF.type, oasis.Oasis))
    graph.add((URIRef(oasisuri), oasis.id , Literal(oasisid)))


    oasilocationsuri = 'http://oasis.senckenberg.de/ontology/OasisLocation_' + nameoasis

    graph.add((URIRef(oasilocationsuri), RDF.type,  oasis.OasisLocation))
    graph.add((URIRef(oasilocationsuri), oasis.OasisLocation_lat, Literal( p.iloc[a]["lat"])))
    graph.add((URIRef(oasilocationsuri), oasis.OasisLocation_long, Literal(p.iloc[a]["long"])))
    graph.add((URIRef(oasilocationsuri), oasis.OasisLocation_country, Literal(p.iloc[a]["Country"])))
    graph.add((URIRef(oasilocationsuri), rdfs.label ,Literal( p.iloc[a]["Oases"])))

    graph.add((URIRef(oasisuri), oasis.hasLocation, URIRef(oasilocationsuri)))

    row = n[n["id"] == oasisid]

    if row.empty:
      print("No matching OpenAlex row found for:", oasisid)
    
    if not row.empty:

        row = row.iloc[0]

        oasistopicsuri = 'http://oasis.senckenberg.de/ontology/OasisTopics_' + nameoasis

        graph.add((URIRef(oasistopicsuri), RDF.type, oasis.OasisTopics))
        graph.add((URIRef(oasisuri), oasis.hasTopics, URIRef(oasistopicsuri)))


        for k in range(len(row["topics"])):

            if row["topics"][k]['type'] == 'subfield' :

                disciplineuri = 'http://oasis.senckenberg.de/ontology/OasisTopicsDiscipline_' + nameoasis
                graph.add((URIRef(disciplineuri), RDF.type, oasis.OasisTopicsDiscipline))

                disciplinescore = str(row["topics"][k]['display_name']) + ':' + str(row["topics"][k]['score'])
                graph.add((URIRef(disciplineuri), oasis.OasisTopicsDiscipline_score , Literal(disciplinescore )))
                graph.add((URIRef(disciplineuri), rdfs.label , Literal(row["topics"][k]['display_name'] )))

                graph.add((URIRef(oasistopicsuri), oasis.hasDiscipline, URIRef(disciplineuri)))

            if row["topics"][k]['type'] == 'topic' :

                focusuri = 'http://oasis.senckenberg.de/ontology/OasisTopicsFocus_' + nameoasis
                graph.add((URIRef(focusuri), RDF.type, oasis.OasisTopicsFocus))

                graph.add((URIRef(focusuri), rdfs.label , Literal(row["topics"][k]['display_name'] )))

                scorevalue = str(row["topics"][k]['display_name']) + ':' + str(row["topics"][k]['score'])
                graph.add((URIRef(focusuri), oasis.OasisTopicsFocus_score , Literal(scorevalue)))

                graph.add((URIRef(oasistopicsuri), oasis.hasFocus, URIRef(focusuri)))

            if row["topics"][k]['type'] == 'sustainable_development_goals' :

                sdguri = 'http://oasis.senckenberg.de/ontology/OasisTopicsSdgs_' + nameoasis
                graph.add((URIRef(sdguri), RDF.type, oasis.OasisTopicsSdgs))

                sdgscore = str(row["topics"][k]['display_name']) + ':' + str(row["topics"][k]['score'])
                graph.add((URIRef(sdguri), oasis.OasisTopicsSdgs_score , Literal(sdgscore )))
                graph.add((URIRef(sdguri), rdfs.label , Literal(row["topics"][k]['display_name'] )))


                graph.add((URIRef(oasistopicsuri), oasis.hasSdg, URIRef(sdguri)))


        oasisrefsuri = 'http://oasis.senckenberg.de/ontology/OasisReferences_' + row["id"]

        graph.add((URIRef(oasisuri), oasis.hasReferences, URIRef(oasisrefsuri)))

        graph.add((URIRef(oasisrefsuri), RDF.type, oasis.OasisReferences))
        nodesuri = 'http://oasis.senckenberg.de/ontology/OasisReferencesNodes_' + row["id"]
        edgesuri = 'http://oasis.senckenberg.de/ontology/OasisReferencesEdges_' + row["id"]

        graph.add((URIRef(oasisrefsuri), oasis.hasNodes, URIRef(nodesuri)))
        graph.add((URIRef(oasisrefsuri), oasis.hasEdges, URIRef(edgesuri)))

        graph.add((URIRef(nodesuri), RDF.type, oasis.OasisReferencesNodes))
        graph.add((URIRef(nodesuri), oasis.OasisReferencesNodes_id, Literal(row["id"])))
        graph.add((URIRef(nodesuri), oasis.OasisReferencesNodes_title, Literal(row["title"])))
        graph.add((URIRef(nodesuri), oasis.OasisReferencesNodes_doi, Literal(row["doi"])))
        graph.add((URIRef(nodesuri), oasis.OasisReferencesNodes_abstract, Literal(row["abstract"])))
        graph.add((URIRef(nodesuri), oasis.OasisReferencesNodes_year, Literal(row["publication_year"])))
        graph.add((URIRef(nodesuri), oasis.OasisReferencesNodes_type, Literal(row["type"])))
        graph.add((URIRef(nodesuri), oasis.OasisReferencesNodes_oa, Literal(row["is_oa"])))

        if row["id"] in oa_id_list:

            graph.add((URIRef(nodesuri), oasis.OasisReferencesNodes_reviewed, Literal('True')))

        else:

            graph.add((URIRef(nodesuri), oasis.OasisReferencesNodes_reviewed, Literal('False')))



        for t in range (len(row["authorships"])):

            authorsname = row["authorships"][t]['display_name']


            uriname = authorsname.replace(' ','_').strip()
            uriname = uriname.replace(',','_')
            uriname = uriname.replace('`','_')
            uriname = uriname.replace('.','_')
            uriname = uriname.replace('\\','')
            uriname = uriname.replace('-','')
            uriname = uriname.replace(')','')
            uriname = uriname.replace('(','')
            uriname = uriname.replace(';','')
            uriname = re.sub(r'<.*?>', '', uriname)


            authorsuri = 'http://oasis.senckenberg.de/ontology/OasisNodesAuthors_' + row["id"]

            graph.add((URIRef(authorsuri), RDF.type, oasis.OasisNodesAuthors))

            if row["authorships"][t]['author_position'] == 'first':

                graph.add((URIRef(authorsuri), oasis.OasisNodesAuthors_position_first, Literal(authorsname)))

            if row["authorships"][t]['author_position'] == 'middle':

                graph.add((URIRef(authorsuri), oasis.OasisNodesAuthors_position_middle, Literal(authorsname)))

            if row["authorships"][t]['author_position'] == 'last':

                graph.add((URIRef(authorsuri), oasis.OasisNodesAuthors_position_last, Literal(authorsname)))

            if row["authorships"][t]['is_corresponding'] == 'True':
                graph.add((URIRef(authorsuri), oasis.OasisNodesAuthors_corresponding, Literal(authorsname)))

            graph.add((URIRef(authorsuri), rdfs.label , Literal(authorsname)))

            graph.add((URIRef(nodesuri), oasis.hasAuthor, URIRef(authorsuri)))


            authoraff = 'http://oasis.senckenberg.de/ontology/OasisNodesAuthorsAffiliation_' + uriname

            graph.add((URIRef(authoraff), RDF.type, oasis.OasisNodesAuthorsAffiliation))

            graph.add((URIRef(authorsuri), oasis.hasAffiliation, URIRef(authoraff)))

            for s in range (len(row["authorships"][t]['affiliations'])):

                graph.add((URIRef(authoraff), oasis.OasisNodesAuthorsAffiliation_country_code, Literal(row["authorships"][t]['affiliations'][s]['country_code'])))
                graph.add((URIRef(authoraff), oasis.OasisNodesAuthorsAffiliation_type, Literal(row["authorships"][t]['affiliations'][s]['type'])))
                graph.add((URIRef(authoraff), rdfs.label, Literal(row["authorships"][t]['affiliations'][s]['display_name'])))


        sourceuri = 'http://oasis.senckenberg.de/ontology/OasisNodesSource_' + row["id"]
        graph.add((URIRef(sourceuri), RDF.type, oasis.OasisNodesSource))
        if row["source_id"] != None:
            graph.add((URIRef(sourceuri), oasis.OasisNodesSource_url , URIRef(row["source_id"])))
        graph.add((URIRef(sourceuri), rdfs.label , Literal(row["source_display_name"])))



    for w in range(len(e)):

        graph.add((URIRef(edgesuri), RDF.type, oasis.OasisReferencesEdges))

        graph.add((URIRef(edgesuri), oasis.OasisReferencesEdges_from, Literal(e.iloc[w]["from"]) ))

        graph.add((URIRef(edgesuri), oasis.OasisReferencesEdges_to , Literal(e.iloc[w]["to"])))

    counter += 1

    if counter % 200 == 0:

        if first_write:
            graph.serialize(destination='output/GOKH.ttl', format="turtle")
            first_write = False
        else:
            with open('output/GOKH.ttl', 'a', encoding='utf-8') as f:
                f.write(graph.serialize(format="turtle"))

        graph = Graph()

        graph.bind('skos', skos)
        graph.bind('foaf', foaf)
        graph.bind('oasis', oasis)
        graph.bind('gndo', gndo)
        graph.bind('owl', owl)
        graph.bind('edm', edm)
        graph.bind('dc', dc)
        graph.bind('dcterms', dcterms)
        graph.bind('rdfs', rdfs)
        graph.bind("xsd", xsd)


# final write
if first_write:
    graph.serialize(destination='output/GOKH.ttl', format="turtle")
else:
    with open('output/GOKH.ttl', 'a', encoding='utf-8') as f:
        f.write(graph.serialize(format="turtle"))

with open("output/GOKH.ttl", "rb") as f_in:
    with gzip.open("output/GOKH.ttl.gz", "wb") as f_out:
        shutil.copyfileobj(f_in, f_out)        
        
        
        
