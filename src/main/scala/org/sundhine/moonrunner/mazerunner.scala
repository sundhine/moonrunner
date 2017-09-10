package org.sundhine.moonrunner
import monocle.macros.syntax.lens._

object mazerunner {

  type Maze = Map[Page, BaseNode]
  type Page = Int
  type Path = List[Page]
  type Choices = List[Page]

  case class Stats(skill: Int, stamina: Int, luck: Int)

  def generateStats: Stats = {
    val r = scala.util.Random
    Stats(skill = r.nextInt(6) + 7,
      stamina = r.nextInt(6) + r.nextInt(6) + 14,
      luck = r.nextInt(6) + 7)
  }

  case class AdventureSheet(
                           lugosh : Boolean = false
                           )


  case class State(stats: Stats, adventureSheet: AdventureSheet)

  def newState: State = State(stats = generateStats, adventureSheet = AdventureSheet())

  //Write on adventure sheet
  def lugoshPriestgate(state: State): State = state.lens(_.adventureSheet.lugosh).modify(_ => true)


  sealed trait BaseNode

  case object VictoryNode extends BaseNode

  sealed abstract class StoryNode extends BaseNode {
    def update(state: State): State

    def choices(state: State): Choices
  }


  def choiceNode(nodes: Page*): BaseNode = new StoryNode {
    override def update(state: State): State = state

    override def choices(state: State): Choices = nodes.toList
  }

  def updateWithChoiceNode(updateFn: (State) => State, nodes: Page*): StoryNode = new StoryNode {
    override def update(state: State): State = updateFn(state)

    override def choices(state: State): Choices = nodes.toList
  }

  def optionalChoice(choiceFn: State => Choices):StoryNode = new StoryNode {
    override def update(state: State): State = state

    override def choices(state: State): Choices = choiceFn(state)
  }

  def failureNode: BaseNode = new StoryNode {
    override def update(state: State): State = state

    override def choices(state: State): Choices = Nil
  }

  def victoryNode: BaseNode = VictoryNode

  case class Result(state: State, path: Path) {
    import sext._
    override def toString: String = {
      s"Final State: ${state.valueTreeString}\nFinal Path: ${path.mkString("[",",","]")}"
    }
  }


  private def iSolveMaze(maze: Maze, state: State, page: Page, path: Path, visited: Set[Page]): Option[Result] = {
    if (visited.contains(page)) None
    else maze(page) match {
      case VictoryNode => Some(Result(state, (page :: path).reverse))
      case node: StoryNode =>
        val newState = node.update(state)
        node
          .choices(newState)
          .toStream
          .flatMap(nextPage => iSolveMaze(maze, newState, nextPage, page :: path, visited + page))
          .headOption
    }
  }

  def solveMaze(maze: Maze, state: State): Option[Result] = iSolveMaze(maze, state, 1, Nil, Set.empty)

}

object maze {

  import mazerunner._


  val maze = Map(
    1 -> choiceNode(12, 33, 80),
    2 -> choiceNode(205, 44),
    3 -> choiceNode(291, 339),
    4 -> choiceNode(293),
    5 -> choiceNode(155, 236),
    6 -> choiceNode(170, 140, 391),
    7 -> choiceNode(275, 287, 117, 58),
    8 -> failureNode,
    9 -> choiceNode(398, 46),
    10 -> choiceNode(341, 130, 200),
    11 -> choiceNode(85, 200),
    12 -> updateWithChoiceNode(lugoshPriestgate, 149, 80),
    13 -> choiceNode(387, 70, 361),
    14 -> choiceNode(37),
    15 -> choiceNode(138, 182),
    16 -> choiceNode(350, 165, 308),
    17 -> choiceNode(45, 368),
    18 -> choiceNode(220, 275, 287, 117, 58),
    19 -> choiceNode(164, 215, 280),
    20 -> failureNode,
    21 -> choiceNode(397, 48, 158, 286),
    22 -> choiceNode(322, 66, 254, 81),
    23 -> choiceNode(2, 44),
    24 -> choiceNode(352, 312),
    25 -> choiceNode(251, 277),
    26 -> choiceNode(110),
    27 -> failureNode,
    28 -> choiceNode(364, 38),
    29 -> choiceNode(349, 298, 59, 153),
    30 -> choiceNode(147, 169, 147),
    31 -> choiceNode(200),
    32 -> choiceNode(225, 133, 271),
    33 -> choiceNode(189, 55),
    34 -> choiceNode(185, 250),
    35 -> choiceNode(234, 118),
    36 -> failureNode,
    37 -> choiceNode(329, 26),
    38 -> choiceNode(98, 202),
    39 -> choiceNode(389, 201),
    40 -> choiceNode(306, 366),
    41 -> choiceNode(236, 5, 195),
    42 -> choiceNode(63, 333),
    43 -> choiceNode(248, 200),
    44 -> choiceNode(357, 393, 207),
    45 -> choiceNode(347, 255),
    46 -> choiceNode(87),
    47 -> failureNode,
    48 -> failureNode,
    49 -> choiceNode(24, 136),
    50 -> choiceNode(78, 295, 200),
    51 -> choiceNode(84, 383),
    52 -> choiceNode(86, 253),
    53 -> choiceNode(200),
    54 -> choiceNode(369),
    55 -> choiceNode(319, 12, 80),
    56 -> choiceNode(141, 17),
    57 -> choiceNode(399, 343, 8),
    58 -> choiceNode(318, 348),
    59 -> choiceNode(103, 153),
    60 -> choiceNode(143, 20),
    61 -> choiceNode(11, 390),
    62 -> choiceNode(248, 200),
    63 -> choiceNode(142, 276),
    64 -> choiceNode(391, 140, 6),
    65 -> choiceNode(272, 272, 101),
    66 -> choiceNode(222, 81),
    67 -> choiceNode(217, 188, 300),
    68 -> choiceNode(115, 137, 41),
    69 -> choiceNode(90),
    70 -> choiceNode(330, 361, 387),
    71 -> choiceNode(260, 383),
    72 -> choiceNode(374, 28),
    73 -> choiceNode(131, 362),
    74 -> choiceNode(178, 231, 95),
    75 -> choiceNode(86, 253),
    76 -> choiceNode(19),
    77 -> choiceNode(172),
    78 -> choiceNode(198, 295, 200),
    79 -> choiceNode(315, 200),
    80 -> choiceNode(377, 252, 149),
    81 -> failureNode,
    82 -> choiceNode(387, 361),
    83 -> choiceNode(208, 249, 122),
    84 -> failureNode,
    85 -> choiceNode(364, 38),
    86 -> choiceNode(345, 173, 253),
    87 -> choiceNode(167, 379, 268),
    88 -> choiceNode(19),
    89 -> failureNode,
    90 -> choiceNode(325, 153),
    91 -> choiceNode(43),
    92 -> choiceNode(200),
    93 -> choiceNode(39, 201, 389),
    94 -> choiceNode(3, 383),
    95 -> choiceNode(178, 231),
    96 -> choiceNode(230, 35),
    97 -> choiceNode(393, 57, 207),
    98 -> choiceNode(123, 338),
    99 -> victoryNode,
    100 -> choiceNode(238, 226),
    101 -> choiceNode(101),
    102 -> choiceNode(330, 361, 387),
    103 -> choiceNode(90, 153),
    104 -> choiceNode(175),
    105 -> choiceNode(208, 249, 122),
    106 -> choiceNode(50, 198, 314),
    107 -> choiceNode(360, 223),
    108 -> choiceNode(139, 251, 277),
    109 -> choiceNode(288, 235, 367, 200),
    110 -> choiceNode(344, 200),
    111 -> choiceNode(214, 4, 350, 165),
    112 -> choiceNode(138, 182),
    113 -> choiceNode(285),
    114 -> choiceNode(83, 304, 237),
    115 -> choiceNode(211, 137, 328),
    116 -> choiceNode(369),
    117 -> choiceNode(220, 275, 287, 58),
    118 -> choiceNode(234, 129),
    119 -> choiceNode(399),
    120 -> choiceNode(200, 392),
    121 -> choiceNode(393, 57, 207),
    122 -> failureNode,
    123 -> choiceNode(386, 141, 17),
    //Lugosh is done, not the windmill
    124 -> optionalChoice(st => List(393, 23, 206) ++ (if (st.adventureSheet.lugosh) List(64) else Nil)),
    125 -> choiceNode(35, 251, 277),
    126 -> choiceNode(200, 282),
    127 -> choiceNode(200),
    128 -> choiceNode(172),
    129 -> choiceNode(181),
    130 -> choiceNode(78, 154),
    131 -> choiceNode(362, 8),
    132 -> choiceNode(114, 199),
    133 -> choiceNode(51, 225),
    134 -> failureNode,
    135 -> choiceNode(157, 192, 212),
    136 -> choiceNode(187, 209),
    137 -> choiceNode(180, 89),
    138 -> choiceNode(299),
    139 -> choiceNode(74, 251, 277),
    140 -> choiceNode(256, 391, 170),
    141 -> choiceNode(171, 196),
    142 -> choiceNode(369),
    143 -> choiceNode(301, 393, 23, 207),
    144 -> choiceNode(156),
    145 -> choiceNode(120, 92),
    146 -> choiceNode(297, 239),
    147 -> choiceNode(147),
    148 -> choiceNode(359, 26),
    149 -> choiceNode(223, 107, 278),
    150 -> choiceNode(393, 57, 207),
    151 -> failureNode,
    152 -> choiceNode(220, 275, 287, 117, 58),
    153 -> choiceNode(114),
    154 -> choiceNode(106, 198, 314),
    155 -> choiceNode(281),
    156 -> choiceNode(19),
    157 -> choiceNode(274, 177),
    158 -> choiceNode(65, 21, 397, 48, 286),
    159 -> choiceNode(31, 380, 200),
    160 -> choiceNode(26, 14, 148),
    161 -> choiceNode(386, 141, 17),
    162 -> choiceNode(193, 47),
    163 -> choiceNode(346, 266, 137, 328),
    164 -> failureNode,
    165 -> choiceNode(370, 350),
    166 -> choiceNode(260, 383),
    167 -> choiceNode(379, 268),
    168 -> choiceNode(92),
    169 -> choiceNode(388, 99),
    170 -> choiceNode(97),
    171 -> choiceNode(79, 216),
    172 -> choiceNode(183, 302),
    173 -> choiceNode(307, 331),
    174 -> choiceNode(374, 28),
    175 -> choiceNode(94, 3, 383),
    176 -> choiceNode(284, 53, 200),
    177 -> choiceNode(274),
    178 -> choiceNode(95, 231),
    179 -> choiceNode(35),
    180 -> choiceNode(363, 309),
    181 -> choiceNode(369),
    182 -> failureNode,
    183 -> choiceNode(57, 207),
    184 -> choiceNode(353, 9),
    185 -> failureNode,
    186 -> choiceNode(200),
    187 -> choiceNode(352, 312),
    188 -> choiceNode(30),
    189 -> choiceNode(93, 149, 80),
    190 -> choiceNode(200),
    191 -> choiceNode(35),
    192 -> choiceNode(274, 177),
    193 -> choiceNode(220, 275, 117, 58),
    194 -> choiceNode(345, 290, 253),
    195 -> choiceNode(301, 236),
    196 -> choiceNode(171),
    197 -> choiceNode(108, 384),
    198 -> choiceNode(295, 200),
    199 -> choiceNode(400),
    200 -> choiceNode(340, 61, 334, 10, 385, 245, 75),
    201 -> choiceNode(16),
    202 -> choiceNode(98, 113),
    203 -> choiceNode(275, 287, 117, 58),
    204 -> choiceNode(26, 14, 148),
    205 -> choiceNode(44),
    206 -> choiceNode(191, 296),
    207 -> choiceNode(184, 9),
    208 -> choiceNode(300, 122),
    209 -> failureNode,
    210 -> choiceNode(358, 159),
    211 -> choiceNode(346, 137, 328),
    212 -> choiceNode(243),
    213 -> choiceNode(71, 247, 394, 383),
    214 -> choiceNode(350, 165),
    215 -> choiceNode(29),
    216 -> choiceNode(315, 200),
    217 -> choiceNode(188, 300),
    218 -> choiceNode(108, 384),
    219 -> choiceNode(19),
    220 -> choiceNode(203, 264),
    221 -> choiceNode(200),
    222 -> choiceNode(81, 294, 127),
    223 -> choiceNode(124),
    224 -> choiceNode(132, 114, 199),
    225 -> choiceNode(104),
    226 -> choiceNode(315, 200),
    227 -> choiceNode(399),
    228 -> choiceNode(190, 134),
    229 -> choiceNode(39, 201),
    230 -> choiceNode(206, 296),
    231 -> choiceNode(306, 366, 40),
    232 -> choiceNode(36, 152),
    233 -> choiceNode(157, 192, 212),
    234 -> choiceNode(234),
    235 -> choiceNode(49, 200),
    236 -> choiceNode(60, 143),
    237 -> choiceNode(105),
    238 -> choiceNode(270, 79),
    239 -> choiceNode(398, 46),
    240 -> choiceNode(99, 372),
    241 -> choiceNode(262, 39, 201),
    242 -> choiceNode(321, 188, 300),
    243 -> choiceNode(120, 92),
    244 -> choiceNode(342, 88),
    245 -> choiceNode(26, 14, 148, 395),
    246 -> choiceNode(220, 275, 287, 117),
    247 -> choiceNode(394, 383),
    248 -> choiceNode(200),
    249 -> choiceNode(300),
    250 -> choiceNode(185, 375, 227, 399),
    251 -> choiceNode(52),
    252 -> choiceNode(41, 292, 393, 23, 207),
    253 -> choiceNode(219, 376),
    254 -> choiceNode(294, 66),
    255 -> choiceNode(100, 326, 386, 141),
    256 -> choiceNode(97),
    257 -> choiceNode(188, 300),
    258 -> choiceNode(49, 200),
    259 -> choiceNode(21, 397, 48, 158),
    260 -> choiceNode(121, 104, 32),
    261 -> choiceNode(243),
    262 -> choiceNode(229, 27),
    263 -> choiceNode(160, 371, 204),
    264 -> choiceNode(7),
    265 -> choiceNode(132, 114, 199),
    266 -> choiceNode(211, 137, 328),
    267 -> failureNode,
    268 -> choiceNode(393, 57),
    269 -> choiceNode(200),
    270 -> choiceNode(216),
    271 -> choiceNode(225, 133, 104, 121),
    272 -> choiceNode(244, 76),
    273 -> choiceNode(188, 300),
    274 -> choiceNode(261, 177),
    275 -> choiceNode(172),
    276 -> choiceNode(54),
    277 -> choiceNode(86, 253),
    278 -> choiceNode(336, 223, 107),
    279 -> choiceNode(62, 91, 43),
    280 -> choiceNode(29),
    281 -> choiceNode(393, 23, 207),
    282 -> choiceNode(197, 218),
    283 -> choiceNode(25, 251, 277),
    284 -> choiceNode(228, 190),
    285 -> choiceNode(98, 123),
    286 -> choiceNode(317, 327),
    287 -> choiceNode(77, 365, 162),
    288 -> choiceNode(235, 221),
    289 -> choiceNode(168, 145),
    290 -> failureNode,
    291 -> choiceNode(84, 383),
    292 -> choiceNode(68, 396),
    293 -> choiceNode(165, 308),
    294 -> failureNode,
    295 -> choiceNode(269),
    296 -> choiceNode(369, 35),
    297 -> choiceNode(239, 324),
    298 -> choiceNode(265, 69),
    299 -> choiceNode(182, 279),
    300 -> choiceNode(335, 67, 242, 257, 356, 273, 30),
    301 -> choiceNode(394, 213, 383),
    302 -> choiceNode(183),
    303 -> choiceNode(200),
    304 -> choiceNode(208, 249, 122),
    305 -> choiceNode(22, 81),
    306 -> choiceNode(40, 366),
    307 -> choiceNode(345, 290, 253),
    308 -> choiceNode(111, 214, 4),
    309 -> choiceNode(2, 44),
    310 -> choiceNode(240, 267),
    311 -> choiceNode(82),
    312 -> failureNode,
    313 -> choiceNode(333, 42),
    314 -> choiceNode(78, 295, 200),
    315 -> choiceNode(186, 151, 303),
    316 -> choiceNode(260, 383),
    317 -> choiceNode(327),
    318 -> choiceNode(246, 128),
    319 -> choiceNode(189, 149),
    320 -> choiceNode(393, 57, 207),
    321 -> choiceNode(188, 300),
    322 -> choiceNode(200),
    323 -> choiceNode(200),
    324 -> choiceNode(398, 393, 57),
    325 -> choiceNode(132, 114, 199),
    326 -> choiceNode(386, 238),
    327 -> choiceNode(259, 21, 397, 48, 158),
    328 -> choiceNode(41, 393, 23),
    329 -> choiceNode(110),
    330 -> choiceNode(82, 311, 82, 102),
    331 -> choiceNode(194, 290),
    332 -> choiceNode(57, 207),
    333 -> choiceNode(116, 35, 179),
    334 -> choiceNode(210, 380),
    335 -> choiceNode(188, 300),
    336 -> choiceNode(107, 223),
    337 -> choiceNode(139, 251, 277),
    338 -> failureNode,
    339 -> choiceNode(84, 383),
    340 -> choiceNode(258, 109, 200),
    341 -> choiceNode(130, 200),
    342 -> failureNode,
    343 -> choiceNode(34, 375, 227, 399),
    344 -> choiceNode(176, 200),
    345 -> choiceNode(73),
    346 -> choiceNode(363, 309),
    347 -> choiceNode(326, 386, 141),
    348 -> choiceNode(36, 220, 275, 287, 117),
    349 -> choiceNode(224, 153),
    350 -> choiceNode(293),
    351 -> choiceNode(168, 145),
    352 -> failureNode,
    353 -> choiceNode(9, 146),
    354 -> choiceNode(98, 202),
    355 -> choiceNode(166, 373),
    356 -> choiceNode(188, 300),
    357 -> choiceNode(393, 207),
    358 -> choiceNode(159),
    359 -> choiceNode(110),
    360 -> choiceNode(393, 23, 207),
    361 -> choiceNode(39, 289, 135),
    362 -> choiceNode(54),
    363 -> choiceNode(2, 44),
    364 -> choiceNode(161, 354),
    365 -> choiceNode(220, 275, 117, 58),
    366 -> choiceNode(25, 283),
    367 -> choiceNode(200),
    368 -> choiceNode(347, 255),
    369 -> choiceNode(21, 397, 48, 158, 286),
    370 -> choiceNode(320, 150),
    371 -> choiceNode(110),
    372 -> failureNode,
    373 -> failureNode,
    374 -> choiceNode(85, 200),
    375 -> choiceNode(119, 185),
    376 -> choiceNode(313, 96, 362),
    377 -> choiceNode(41, 292, 393, 23, 207),
    378 -> choiceNode(73, 144),
    379 -> choiceNode(167, 268),
    380 -> choiceNode(112, 15, 279),
    381 -> choiceNode(21, 48, 158, 286),
    382 -> choiceNode(18),
    383 -> choiceNode(393, 57, 207),
    384 -> choiceNode(337, 251, 277),
    385 -> choiceNode(305, 81, 22),
    386 -> choiceNode(56, 141, 17),
    387 -> choiceNode(13, 145, 351, 233),
    388 -> choiceNode(240, 310),
    389 -> choiceNode(262, 241),
    390 -> choiceNode(72, 11, 174),
    391 -> choiceNode(140, 170),
    392 -> choiceNode(126),
    393 -> choiceNode(232, 382, 332),
    394 -> choiceNode(316, 355),
    395 -> choiceNode(263, 371, 204),
    396 -> choiceNode(266, 137, 163),
    397 -> choiceNode(381, 21, 48, 158, 286),
    398 -> choiceNode(87),
    399 -> choiceNode(387, 361, 70),
    400 -> choiceNode(114),
  )

  def main(args: Array[String]): Unit = {
    println(solveMaze(maze, newState).get)
  }
}
