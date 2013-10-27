function result = findWordProbs(text)


wordCounts = zeros(1,1);

h = waitbar(0);

allWords = {text{1, 1}};
wordCounts(1, 1) = 1;

for i=2:length(text)
    word = text{i,1};
    fprintf(strcat('Processing: ', word, '\n'));
    if(ismember(word, allWords))
        wordIndex = strmatch(word, allWords, 'exact');
        wordCounts(wordIndex) = wordCounts(wordIndex)+1;
    else
        allWords = [allWords, word];
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