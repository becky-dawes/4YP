function wordAndProb = findBigramProb(word, wordPairProbs, wordProbs)

words = wordProbs(1, 1:length(wordProbs));
col = strmatch(word, words, 'exact');
pNMinusOne = wordProbs{2, col};

matchingWordPairIndex = 0;
matchFound = false;

for i=length(wordPairProbs):-1:1
    if ~matchFound
        matchIndex = strfind(wordPairProbs{1,i}, word);
        if matchIndex == 1
            matchingCell = wordPairProbs{1,i};
            if matchingCell(length(word)+1) == ' '
                matchingWordPairIndex = i;
                matchFound = true;
            end
        end
    end
end

pNMinusOneN = 0;
wordAndProb = cell(2,1);

if matchingWordPairIndex ~= 0
    pNMinusOneN = wordPairProbs{2, matchingWordPairIndex};
    wordAndProb{2,1} = pNMinusOneN/pNMinusOne;
    wordAndProb{1,1} = wordPairProbs{1, matchingWordPairIndex};
end

end




