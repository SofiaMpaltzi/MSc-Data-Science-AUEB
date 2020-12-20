DROP FUNCTION SofiaBaltzi_cossim(STRING, STRING);
CREATE FUNCTION SofiaBaltzi_cossim(titles STRING, query STRING)
RETURNS FLOAT
LANGUAGE PYTHON
{
    from sklearn.metrics.pairwise import cosine_similarity
    from sklearn.feature_extraction.text import CountVectorizer
    clf = CountVectorizer(strip_accents='unicode')
    title_vectors = cv.fit_transform(titles)
    query_vectors = cv.transform([query])
    
    return cosine_similarity(query_vectors, title_vectors)
};
