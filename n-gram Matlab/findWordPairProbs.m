function result = findWordPairProbs(text)


wordCounts = zeros(1,1);

h = waitbar(0);

firstPair = strcat(text{1, 1}, {' '}, text{2, 1});
allWords = {firstPair{1,1}};
wordCounts(1, 1) = 1;

previousWord = text{2, 1};

for i=3:length(text)
    word = text{i,1};
    wordPair = strcat(previousWord, {' '}, word);
    fprintf(strcat('Processing: ', wordPair{1,1}, '\n'));
    if(ismember(wordPair{1,1}, allWords))
        wordIndex = strmatch(wordPair{1,1}, allWords, 'exact');
        wordCounts(wordIndex) = wordCounts(wordIndex)+1;
    else
        allWords = [allWords, wordPair{1,1}];
        wordCounts = [wordCounts, 1];
%         tempAllWords = cell(length(allWords)+1);
%         tempWordCounts = zeros(1, length(wordCounts)+1);
%         for j=1:length(allWords)
%             tempAllWords{j} = allWords{j};
%             tempWordCounts(j) = wordCounts(j);
%         end
%         allWords = tempAllWords;
%         wordCounts = tempWordCounts;
%         allWords{length(allWords)-1} = text{i};
%         wordCounts(length(wordCounts)-1) = 1;
    end
    previousWord = word;
    percentage = i/(length(text));
    waitbar(percentage, h, sprintf('Counting word occurrences: %d%%', percentage*100));
end
delete(h);
totalNumWords = length(allWords);

wordProbs = wordCounts/totalNumWords;

orderedWords = cell(1, length(allWords));
orderedProbs = zeros(1, length(wordProbs));
result = cell(2, length(orderedWords));

h = waitbar(0);

for i=1:length(allWords)
    [minProb, minProbIndex] = min(wordProbs);
    orderedProbs(1, i) = wordProbs(1, minProbIndex);
    wordProbs(1, minProbIndex) = inf;
    orderedWords{1, i} = allWords{1, minProbIndex};
    result{1,i} = orderedWords{1,i};
    result{2,i} = orderedProbs(1,i);
    percentage = i/(length(allWords));
    waitbar(percentage, h, sprintf('Ordering words by probability: %d%%', percentage*100));
end
delete(h);

bar(orderedProbs);
set(gca,'XTickLabel',orderedWords);


% h = waitbar(0);
% 
% individualProbs = zeros(1,1);
% wordsByProb = cell(1);
% 
% for i=1:length(orderedWords)
%     boolIndex = strcmp(orderedProbs(i), individualProbs);
%     if(boolIndex)
%         wordIndex = find(boolIndex);
%         wordsByProb{wordIndex} = strcat(wordsByProb{wordIndex}, ', ', orderedWords{i});
%         
%     else
%         tempWordsByProb = cell(length(wordsByProb)+1);
%         tempIndividualProbs = zeros(1, length(individualProbs)+1);
%         for j=1:length(wordsByProb)
%             tempWordsByProb{j} = wordsByProb{j};
%             tempIndividualProbs(j) = individualProbs(j);
%         end
%         wordsByProb = tempWordsByProb;
%         individualProbs = tempIndividualProbs;
%         wordsByProb{length(wordsByProb)-1} = orderedWords{i};
%         individualProbs(length(individualProbs)-1) = orderedProbs(i);
%     end
%     percentage = i/(length(orderedWords));
%     waitbar(percentage, h, sprintf('Splitting up words by probability: %d%%', percentage*100));
% end
% 
% delete(h);
% 
% bar(individualProbs);
% set(gca,'XTickLabel',wordsByProb);

end