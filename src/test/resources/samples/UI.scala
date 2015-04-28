package moviemetase
package ui

import scala.swing._
import scala.swing.Swing._
import scala.swing.event.Event
import java.awt.{Toolkit, Desktop, EventQueue}
import java.net.URL
import java.awt.Color
import javax.swing.BorderFactory
import javax.swing.border.EtchedBorder
import java.net.URI
import java.net.URLEncoder
import javax.swing.UIManager
import comp._

// 11096:../data/crawld/scala/hotzen/moviemetase/src/main/moviemetase/ui/UI.scala

object UI {
  
  def show(): Unit = {
    // Look & Feel
    try UIManager setLookAndFeel "javax.swing.plaf.nimbus.NimbusLookAndFeel" 
    catch { case e:Exception => UIManager setLookAndFeel UIManager.getSystemLookAndFeelClassName }
        
    // Apple
    val props = System.getProperties
    props.setProperty("apple.laf.useScreenMenuBar", "true")
    props.setProperty("com.apple.mrj.application.apple.menu.about.name", App.name + " " + App.version)
    
    Swing.onEDT {
      val top = new UI
      top.iconImage = UI.toolkit.getImage( App.resource("/img/icon.png") )
                  
      val screenSize = toolkit.getScreenSize
      top.size = new Dimension(
        (screenSize.width  * 0.8).toInt,
        (screenSize.height * 0.8).toInt
      )
      
      top.pack()
      top.visible = true
    }
  }
  
  // run in Event-Dispatch-Thread
  def run(block: =>Any): Unit =
    if (EventQueue.isDispatchThread)
      block
    else
      EventQueue.invokeLater(new Runnable {
        def run(): Unit = { block }
      })

  // publish in EDT
  def publish(pub: Publisher)(evt: Event): Unit =
    run { pub publish evt }
    
  val toolkit = Toolkit.getDefaultToolkit
  
  val desktop: Option[Desktop] =
    if (Desktop.isDesktopSupported)
      Some( Desktop.getDesktop )
    else
      None
  
  val UNCRepl =  // Regex-Masked, replaceAll works on Regexes!
    ("#"   -> "%23") ::
    (" "   -> "%20") ::
    ("\\[" -> "%5B") ::
    ("\\]" -> "%5D") ::
    Nil

  def openFile(f: java.io.File) {
    val path = f.getPath
    desktop.foreach(dsk => {
      if (path.startsWith("\\")) { // UNC network path
        val newPath = "file:" + UNCRepl.foldLeft(path)({ case (path, (from, to)) =>
          path.replaceAll(from, to)
        })
        dsk browse new URL(newPath).toURI
      } else {
        dsk open f
      }
    })
  }
  
  def openBrowser(uri: java.net.URI) {
    for (dsk <- desktop)
      dsk browse uri
  }
      
  val SelectionColor = new Color(139, 209, 46) // #8BD12E
  //val SelectionBorder = BorderFactory.createMatteBorder(3, 3, 3, 3, SelectionColor)
}

class UI extends Frame {
    
  val dropPanel     = new DropPanel(this)
  val searchesPanel = new SearchesPanel(this)
  val moviesPanel   = new MoviesPanel(this)
  val infosPanel    = new InfosPanel(this)
  val logPanel      = new LogPanel(this)
  val statusBar     = new StatusBar(this)
  
  contents = new MigPanel("fill") {
    border = Swing.EmptyBorder(5, 5, 5, 5)
        
    val leftPanel = new MigPanel("fill", "center") {
      add(dropPanel, "width 300, center, dock north")
      add(searchesPanel, "width 300, grow")
    }
    
    val rightPanel = new SplitPane {
      resizeWeight = 0.97
      oneTouchExpandable = true
      
      topComponent  = new SplitPane {
        resizeWeight = 0.5 // auto-resize even
        oneTouchExpandable = false
        
        topComponent    = moviesPanel
        bottomComponent = infosPanel
      }
      bottomComponent = logPanel
    } 
    
    add(new SplitPane(Orientation.Vertical, leftPanel, rightPanel), "grow")

    add(statusBar, "dock south, grow, height 25!")
  }
  
  title = App.name + " " + App.version
  iconImage = UI.toolkit.getImage("/img/icon.png")
    
  override def closeOperation = App.shutdown()
  
  
  listenTo( HumanTasks )
  reactions += {
    case e@sites.Google.CaptchaRequired(challenge, _) => {
      import java.awt._
      import javax.swing._
      
      val panel = new JPanel(new BorderLayout)
              
      val lbl = new JLabel()
      lbl.setText("Please enter the CAPTCHA:")
      panel.add(lbl, BorderLayout.NORTH)
      
      val img = new JImage(challenge.img, None, JImage.Blocking, JImage.NoCaching)
      panel.add(img, BorderLayout.SOUTH)
      
      var prompt: String = null
      while (prompt == null) {
        prompt = JOptionPane.showInputDialog(this, panel)
      }
      
      val resp = sites.Google.CaptchaResponse(prompt, challenge)
      println("replying: " + resp)
      e feedback resp        
    }
  }
  
  
  
  { // TEST
    val infos =
      MovieInfos.Title("Inception") ::
      MovieInfos.Release(2010) :: 
      MovieInfos.Genre("Drama") ::
      MovieInfos.Genre("SciFi") ::
      MovieInfos.Genre("Doener") ::
      MovieInfos.Genre("Thriller") ::
      MovieInfos.Actor("Leonardo DiCaprio", Some("Cobb")) ::
      MovieInfos.Actor("Joseph Gordon-Levitt", Some("Arthur")) ::
      MovieInfos.Director("Christopher Nolan") ::
      MovieInfos.IMDB("tt1375666") :: 
      MovieInfos.TMDB("27205") ::
      MovieInfos.Tagline("In a world where technology exists to enter the human mind through dream invasion, a highly skilled thief is given a final chance at redemption which involves executing his toughest job to date: Inception.") ::
      MovieInfos.Poster(new URL("http://ia.media-imdb.com/images/M/MV5BMjAxMzY3NjcxNF5BMl5BanBnXkFtZTcwNTI5OTM0Mw@@._V1._SX640_SY948_.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/ajRNzCiXHBMYmA6kswjMyiqRLVT.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/uUoE76P0bkyBpDKcAY0B5SE2EnR.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/ziKvu3Th9l1wN2aIeVj5ElpBqFu.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/CcyCCPE6CmA4JZNhPyRW0U3oTA.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/oXVyYtb3wizGMmFD1mFcYgsVxp9.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/6vye8rTtwosicoy5augplO26KIR.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/ntoBcrQGfQHqAGFVw8uPfNlE2tK.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/bgC4FdiefFHUbIBR3ZpKVEFHVEK.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/5JnWsEWX5Lz7WHtTGz0czgP9zAV.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/aqw3u1lsVY1tHm9C3wIRNcVrvNp.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/36fVpPt6m4zgHrW8NifSKd6EnIF.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/2B63oMkGypPfPRd00xRVj2DGOz6.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/13AA6QVxO5QaxlTlS4NAU5tmjTZ.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/jlYWQTGqr2XmrmPi8RnVXruBxJ1.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/pElT7dN63byJ0jh43xnUEzA5E7U.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/aKcRofvwJXsiwkyK4pNfnXpXgLT.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/IROA97Rw3ahoMNf86HWvVNV52c.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/mhY1uljyn5MvJ2k5JYcDPRQfP2Z.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/tsxJFwA5rKJCS70Cv62TRivwqxJ.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/yGJzImW7aFSOuZ9W0KFFAYQJnZg.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/y7OZLyGlmVyIIJMRBwejqPFFn87.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/JLb2xe5ROe6eHAGHVDJP5Zltoe.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/bNGehW2wIxagZIlZqszECzXZDck.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/te7M9n5vrvyVAggrxk1iDudqA3Y.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/9YL3Frgm8LUYnoWPQXskLYYg5XZ.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/3NWJvg0YHkD6u9R3D6vGLkk6vyc.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/xsUQlUAJbC32efGDYQyw5IkL8Nw.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/2VnwCwBvwYgojCvgEB4wPKsMCvF.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/gBWJdMxiAnAMEUWmPxc30SDfHIx.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/p8aDLIdn8Id4QErfi4PLJs75RaM.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/eNd3v0luM6MKPuQEiY7SrCrk3Vn.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/fBJemQGslDuN9cpeRKAU3b09jBf.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/2FuzG3LGe7vyzxDULbcJ5c6Rh4b.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/1PIccDb9pjRBCrCKIXz1Y8o83DJ.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/rVzwIxLOWXgKq6RNcRfeQKkbS1v.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/rQiERciWEkpZIOuyNIon7jYpyFx.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/7TOVdPCorus0oV2Nn0Yn0cVqIZZ.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/ePQuB4d09Hj2PR44Jlsk6EsbORW.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/81LoaRNZ5gcphscYysrS4uU7Zq0.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/e8MyVAjISnQ8TCcOPQ0Pk2PTe46.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/yDC2NJ1QpRw35furw7wkNbxZuCF.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/948gaXCI1a40aVzh8yQP1RSowoc.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/3s8zBQ1ut41LK1VrnEaBSyquRBH.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/rJfNKsrzvXYsYxHIxEBnbb7DQQs.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/h9SJsh3FdLcghjNbTX3MOH18jIG.jpg") ) ::
      MovieInfos.Poster(new URL("http://cf2.imgobject.com/t/p/original/cmoBCRsF4LZ3I78Lw6ge4Q7ZQrn.jpg") ) ::
      MovieInfos.Backdrop(new URL("http://cf2.imgobject.com/t/p/original/vmcpt1DALqJSLHTOuN6nJSDzupS.jpg")) ::
      MovieInfos.Backdrop(new URL("http://cf2.imgobject.com/t/p/original/hloLHcDVdFoB7eFvLiKpQPqieFP.jpg")) ::
      MovieInfos.Backdrop(new URL("http://cf2.imgobject.com/t/p/original/zHl0p6NGVdJgFAEjtEZKhmq7EvM.jpg")) ::
      Nil
      
    val movie = Movie(infos).get
    
    //val row = SearchRow(false, "term", "dir", "file", "path", movie :: Nil)
    //UI.publish(searchesPanel)( SearchSelected(row) )
  }
}
