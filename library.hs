type Person = String
type Book = String
type Database = [(Person, Book)]

databaseExample :: Database
databaseExample = [ ("Sergio", "O Senhor dos Aneis"),
                ("Andre", "Duna"),
                ("Fernando", "NOathan Strange & mR. Norrell"),
                ("Fernando", "Duna")
              ]

books :: Database -> Person -> [Book]
books [] searched_person = []
books ((person,livro):pls) searched_person | person == searched_person = livro : books pls searched_person
                                             | otherwise = books pls searched_person

books' :: Database -> Person -> [Book]
books' bd searched_person = [liv | (pess,liv) <- bd, pess == searched_person]

emprestimos :: Database -> Book -> [Person]
emprestimos [] searched_book = []
emprestimos ((p1,l1):pls) searched_book | l1 == searched_book = p1 : emprestimos pls searched_book
                                          | otherwise = emprestimos pls searched_book

-- membro :: Database -> Person -> Person
-- membro [] searched_person = []
-- membro ((p1, l):pls) searched_person | p1 == searched_person = p1 : membro pls searched_person
-- | otherwise = membro pls searched_person

-- No caso abaixo é retornado uma lista de person caso uma ou mais persons iguais a procurada esteja na lista
membro :: Database -> Person -> [Person]
membro bd searched_person = [pess | (pess,liv) <- bd, pess == searched_person]
-- Basicamente eu comparei com uma lista vazia para retornar vazio ou verdadeiro
membroBool :: Database -> Person -> Bool
membroBool bd searched_person = [pess | (pess,liv) <- bd, pess == searched_person] /= []

--