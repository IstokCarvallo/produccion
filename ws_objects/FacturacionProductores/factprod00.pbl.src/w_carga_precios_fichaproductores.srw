$PBExportHeader$w_carga_precios_fichaproductores.srw
$PBExportComments$Selección de Fuente / Tablas para Proceso de Consolidación.
forward
global type w_carga_precios_fichaproductores from w_mant_directo
end type
type st_6 from statictext within w_carga_precios_fichaproductores
end type
type st_12 from statictext within w_carga_precios_fichaproductores
end type
type dw_sel_especie from datawindow within w_carga_precios_fichaproductores
end type
type ddlb_fuentes from dropdownlistbox within w_carga_precios_fichaproductores
end type
type dw_paso from datawindow within w_carga_precios_fichaproductores
end type
type st_1 from statictext within w_carga_precios_fichaproductores
end type
type cbx_especie from checkbox within w_carga_precios_fichaproductores
end type
type pb_eli_det from picturebutton within w_carga_precios_fichaproductores
end type
end forward

global type w_carga_precios_fichaproductores from w_mant_directo
integer width = 3291
integer height = 1964
string title = "CARGA PRECIOS FICHA PRODUCTORES"
event ue_borrar_detalle ( )
st_6 st_6
st_12 st_12
dw_sel_especie dw_sel_especie
ddlb_fuentes ddlb_fuentes
dw_paso dw_paso
st_1 st_1
cbx_especie cbx_especie
pb_eli_det pb_eli_det
end type
global w_carga_precios_fichaproductores w_carga_precios_fichaproductores

type variables
//str_mant istr_mant

Integer		ii_codemp, ii_bodega
String			is_zona, is_encuentra
Transaction	sqlproduc	
Boolean		ib_conectado
Date			id_mespro

datawindowchild       dw_especie
uo_especie            iuo_especie
uo_productores        iuo_productor
uo_variedades         iuo_variedad
uo_calibre             iuo_calibre

end variables

forward prototypes
public subroutine habilita_lectura ()
public subroutine traspasa_datos ()
public function boolean conexionexportaciones ()
public function string duplicado (long al_fila)
end prototypes

event ue_borrar_detalle();IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
pb_grabar.Enabled = TRUE
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

public subroutine habilita_lectura ();Integer li_especie
IF cbx_especie.Checked THEN
	li_especie  = -1
ELSE
   li_especie 	= dw_sel_especie.Object.espe_codigo[1]
END IF

IF NOT IsNull(ddlb_fuentes.Text) AND ddlb_fuentes.Text<>"" AND &
	NOT IsNull(li_especie) THEN
	pb_lectura.enabled=true
END IF
end subroutine

public subroutine traspasa_datos ();Integer 		li_especie, li_variedad,  li_espe
String  		ls_productor, ls_variedad, ls_calibre, ls_especie
Long    		ll_detalle,ll_fila, ll_Productor, ll_tempo
Decimal{4} 	ld_precio
Boolean 		lb_aviso
Date			ld_FecIni, ld_Fecter

SELECT max(pate_tempor) into :ll_tempo
FROM dbo.paramtemporada;

SELECT pate_inicio
INTO   :ld_FecIni
FROM dbo.paramtemporada
WHERE pate_tempor=:ll_tempo;

ld_Fecter	=	Today()

IF cbx_especie.Checked THEN
	li_espe  = -1
ELSE
   li_espe 	= dw_sel_especie.Object.espe_codigo[1]
END IF

IF dw_1.RowCount() = 0 THEN
   IF Not ConexionExportaciones() THEN
	   MessageBox("Atención","Error de Conexión ",Exclamation!)
	   RETURN 
   ELSE
	  dw_paso.SetTransObject(sqlproduc)	
	  ll_fila	= dw_Paso.Retrieve(li_espe)
	  IF ll_fila=0 THEN
		  MessageBox("Atención", "No existe Información para parámetros seleccionados",exclamation!) 
	    RETURN
		 
     ELSEIF dw_paso.RowCount() > 0 THEN
		
	     FOR ll_detalle=1 to dw_paso.RowCount()
		      ll_Productor = dw_paso.Object.prod_codigo[ll_detalle]
				ls_productor = dw_paso.Object.prod_nombre[ll_detalle]
		      li_especie   = dw_paso.Object.espe_codigo[ll_detalle]
				ls_especie   = dw_paso.Object.espe_nombre[ll_detalle]
				li_variedad  = dw_paso.Object.vari_codigo[ll_detalle]
				ls_variedad  = dw_paso.Object.vari_nombre[ll_detalle]
				ls_calibre   = dw_paso.Object.ppcd_calibr[ll_detalle]
		      ld_precio    = dw_paso.Object.ppcd_valvta[ll_detalle]
		      		
		      dw_1.InsertRow(0)
				
		      dw_1.Setitem(ll_detalle,"clie_codigo",81)
		      dw_1.Setitem(ll_detalle,"prod_codigo",ll_Productor)
				dw_1.Setitem(ll_detalle,"prod_nombre",ls_productor)
		      dw_1.Setitem(ll_detalle,"espe_codigo",li_especie)
				dw_1.Setitem(ll_detalle,"espe_nombre",ls_especie)
				dw_1.Setitem(ll_detalle,"vari_codigo",li_variedad)
				dw_1.Setitem(ll_detalle,"vari_nombre",ls_variedad)
		      dw_1.Setitem(ll_detalle,"vaca_calibr",ls_calibre)
		      dw_1.Setitem(ll_detalle,"vafp_preuni",ld_precio)
				dw_1.Setitem(ll_detalle,"vafa_fecini",ld_FecIni)
				dw_1.Setitem(ll_detalle,"vafa_fecter",ld_Fecter)
				dw_1.Setitem(ll_detalle,"vafa_secuen",Integer(ll_detalle))				
				
				duplicado(ll_detalle) 
				
	     NEXT
		     dw_1.SetSort("existe D, prod_codigo A, vari_codigo A, vaca_calibr A, vafp_preuni A, vafa_fecini A")
			  dw_1.Sort()
			  pb_eli_det.Enabled = TRUE
	  
      END IF
	END IF
END IF
 
end subroutine

public function boolean conexionexportaciones ();String	ls_Nombre, ls_Usuario, ls_Password
ii_bodega=0

DISCONNECT USING sqlproduc;

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass

ls_Nombre				=	ProfileString("Liquidac.ini", is_Base, "NombreOdbc", "")
sqlproduc.Dbms			= 	lower(ProFileString("Liquidac.ini", is_base, "dbms", "ODBC"))
sqlproduc.ServerName	=	ProfileString("Liquidac.ini", is_Base, "ServerName", "")
sqlproduc.DataBase	=	ProFileString("Liquidac.ini", is_base, "Database", "")
sqlproduc.DbParm		=	"Connectstring='DSN=" + ls_nombre + ";UID=" + ls_Usuario + &
								";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

CONNECT USING sqlproduc;

IF sqlproduc.SQLCode = 0 THEN
	ib_Conectado	=	True
	
	
ELSE
	ib_Conectado	=	False
END IF
RETURN ib_Conectado
end function

public function string duplicado (long al_fila);Long		ll_fila, ll_productor
Integer  li_especie, li_Secuen, li_variedad 
String   ls_calibre 


li_especie 	 = dw_1.Object.espe_codigo[al_fila]
ll_productor =	dw_1.Object.prod_codigo[al_fila]
li_variedad  =	dw_1.Object.vari_codigo[al_fila]
ls_calibre   = dw_1.Object.vaca_calibr[al_fila]
li_Secuen    = dw_1.Object.vafa_secuen[al_fila]

ll_fila	=  dw_1.Find("prod_codigo = " + String(ll_productor) + &
                  "AND espe_codigo = " + String(li_especie) + &
			         "AND vari_codigo = " + String(li_variedad) + &
			         "AND vafa_secuen = " + String(li_Secuen) + &						
			         "AND vaca_calibr = '" + ls_calibre +  "'",1,dw_1.RowCount())


IF ll_fila >0 and ll_fila <> al_fila THEN
	dw_1.Object.existe[al_fila]='SI'
	dw_1.Object.existe[ll_fila]='SI'
	dw_1.SelectRow(al_fila,True)
	dw_1.SelectRow(ll_fila,True)
	is_encuentra = 'SI'
ELSE
	dw_1.Object.existe[al_fila]='NO'
END IF

RETURN is_encuentra




end function

on w_carga_precios_fichaproductores.create
int iCurrent
call super::create
this.st_6=create st_6
this.st_12=create st_12
this.dw_sel_especie=create dw_sel_especie
this.ddlb_fuentes=create ddlb_fuentes
this.dw_paso=create dw_paso
this.st_1=create st_1
this.cbx_especie=create cbx_especie
this.pb_eli_det=create pb_eli_det
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_6
this.Control[iCurrent+2]=this.st_12
this.Control[iCurrent+3]=this.dw_sel_especie
this.Control[iCurrent+4]=this.ddlb_fuentes
this.Control[iCurrent+5]=this.dw_paso
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.cbx_especie
this.Control[iCurrent+8]=this.pb_eli_det
end on

on w_carga_precios_fichaproductores.destroy
call super::destroy
destroy(this.st_6)
destroy(this.st_12)
destroy(this.dw_sel_especie)
destroy(this.ddlb_fuentes)
destroy(this.dw_paso)
destroy(this.st_1)
destroy(this.cbx_especie)
destroy(this.pb_eli_det)
end on

event open;X	= 0
Y	= 0
dw_1.SetTransObject(Sqlca)

dw_sel_especie.Getchild("espe_codigo",dw_especie)
dw_especie.SettransObject(sqlca)
dw_especie.Retrieve()
dw_especie.SetSort("espe_nombre A")
dw_especie.Sort()
dw_sel_especie.SettransObject(sqlca)
dw_sel_especie.InsertRow(0)

dw_sel_especie.Enabled	=	False


String	ls_linea
Integer	li_archivo, li_inicio, li_termino

This.Icon				=	gstr_apl.Icono
li_archivo				=	FileOpen("Liquidac.Ini")
sqlproduc				=	CREATE TRANSACTION

IF li_archivo < 0 THEN
	MessageBox("Error","Archivo " + "Liquidac.Ini" + " no se encuentra en directorio.", StopSign!)
	Close(This) 
	Return
ELSE
	SetPointer(HourGlass!)
	
	ddlb_fuentes.SetRedraw(False)
	ddlb_fuentes.Reset()

	DO WHILE FileRead(li_archivo,ls_linea) >= 0
				
		li_inicio	= Pos(ls_linea,"[",1)
		li_termino	= Pos(ls_linea,"]",1)
		
		IF li_inicio > 0 AND li_termino>0 THEN
			IF Pos(Mid(ls_linea,li_inicio + 1,li_termino - li_inicio -1),gs_base) = 0 THEN
				ddlb_fuentes.AddItem(Mid(ls_linea,li_inicio + 1,li_termino - li_inicio -1))
			END IF
		END IF
	LOOP
	
	FileClose(li_archivo)
	ddlb_fuentes.SetRedraw(True)
	ddlb_fuentes.SelectItem(1)
	is_base	 = ddlb_fuentes.Text
	
END IF
pb_lectura.Enabled   = FALSE
pb_eliminar.Enabled  = FALSE
pb_eli_det.Enabled   = FALSE
SetPointer(Arrow!)
pb_grabar.SetFocus()
dw_1.SetRowFocusIndicator(Hand!)
habilita_lectura()

//IF li_archivo < 0 THEN Close(This)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
Integer li_especie

is_encuentra=""

 IF cbx_especie.Checked THEN
	li_especie  = -1
   ELSE
      li_especie 	= dw_sel_especie.Object.espe_codigo[1]
	   IF IsNull(li_especie) OR li_especie=0 THEN
	      MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	      RETURN
	   END IF
END IF

DO
	ll_fila	= dw_1.Retrieve(li_especie)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		 MessageBox("Atención", "Tabla ya contiene datos debe Eliminar Información para cargar datos") 
       dw_1.SetRow(1)
		 dw_1.SetFocus()
		 il_fila	= 1	
		 pb_grabar.Enabled		= True
		 pb_eliminar.Enabled    = True
	ELSE
		traspasa_datos()
		TriggerEvent("ue_validaregistro")
   END IF	
	
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_nuevo;call super::ue_nuevo;dw_1.SetColumn("vafp_preuni")
end event

event ue_validaregistro;Integer	li_cont,li_cont1
String	ls_mensaje, ls_colu[]
Long     ll_fila

IF dw_1.RowCount() >0 THEN
   FOR ll_fila = 1 TO dw_1.RowCount()
       		 
		 //productor
	    iuo_productor		=	CREATE	uo_productores
	    IF Isnull(dw_1.Object.prod_codigo[ll_Fila]) OR dw_1.Object.prod_codigo[ll_Fila] = 0 THEN
		    li_cont ++
		    ls_mensaje 			= ls_mensaje + "~nCódigo productor"
		    ls_colu[li_cont]	= "prod_codigo"
	    ELSEIF NOT iuo_productor.Existe(dw_1.Object.prod_codigo[ll_Fila]	, FALSE, Sqlca)  THEN
			 li_cont ++
		    ls_mensaje 			= ls_mensaje + "~nCódigo productor,no existe o no corresponde a zona ingresada."
		    ls_colu[li_cont]	= "prod_codigo"
	    END IF							
	    Destroy iuo_productor
						
		
       //especie
	    iuo_especie		=	CREATE	uo_especie
	    IF Isnull(dw_1.Object.espe_codigo[ll_Fila]) OR dw_1.Object.espe_codigo[ll_Fila] = 0 THEN
		    li_cont ++
		    ls_mensaje 			= ls_mensaje + "~nCódigo Especie"
		    ls_colu[li_cont]	= "espe_codigo"
	    ELSEIF NOT iuo_especie.Existe(dw_1.Object.espe_codigo[ll_Fila], FALSE, Sqlca) THEN
		    li_cont ++
		    ls_mensaje 			= ls_mensaje + "~nCódigo Especie, o no ha sido ingresado en tabla respectiva."
		    ls_colu[li_cont]	= "espe_codigo"
	    END IF
	    Destroy iuo_especie
		 
		  //variedad
	    iuo_variedad	   =	CREATE	uo_variedades
	    IF Isnull(dw_1.Object.vari_codigo[ll_Fila]) OR dw_1.Object.vari_codigo[ll_Fila] = 0 THEN
		    li_cont ++
		    ls_mensaje 			= ls_mensaje + "~nCódigo de Variedad " 
		    ls_colu[li_cont]	= "vari_codigo"
	    
	    ELSEIF NOT iuo_variedad.Existe(dw_1.Object.espe_codigo[ll_Fila],dw_1.Object.vari_codigo[ll_Fila], False, Sqlca) THEN
		    li_cont ++
		    ls_mensaje 			= ls_mensaje + "~nCódigo de Variedad " + String(dw_1.Object.vari_codigo[ll_Fila]) + " no ha sido ingresado en tabla respectiva."
		    ls_colu[li_cont]	= "vari_codigo"
	    END IF
	    Destroy iuo_variedad
		 
				 
		 //calibre
	    iuo_calibre	   =	CREATE	uo_calibre
	    IF Isnull(dw_1.Object.vaca_calibr[ll_Fila]) OR dw_1.Object.vaca_calibr[ll_Fila] = "" THEN
		    li_cont ++
		    ls_mensaje 			= ls_mensaje + "~nCódigo de Calibre " 
		    ls_colu[li_cont]	= "vaca_calibr"
	    
	    ELSEIF NOT iuo_calibre.Existe_calibre(dw_1.Object.espe_codigo[ll_Fila],dw_1.Object.vari_codigo[ll_Fila],dw_1.Object.vaca_calibr[ll_Fila], False, Sqlca) THEN
		    li_cont ++
		    ls_mensaje 			= ls_mensaje + "~nCódigo de Calibre " + String(dw_1.Object.vaca_calibr[ll_Fila])+" "+ + "Fila Nº  " + String(ll_fila) + " no ha sido ingresado en tabla respectiva."
			 ls_colu[li_cont]	= "vaca_calibr"
	    END IF
	    Destroy iuo_calibre
		 
		 //EXISTE
		 duplicado(ll_Fila)
	NEXT
END IF

IF li_cont > 0 THEN
	IF is_encuentra = 'SI' then
		MessageBox("Error Llave Duplicada", "Existen Registros Iguales, Verifique",StopSign!, Ok!)		  
	ELSE
	   MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
   END IF
	pb_grabar.Enabled = FALSE
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
ELSE
	pb_grabar.Enabled = TRUE
END IF
end event

event ue_borrar;
Integer li_especie

IF cbx_especie.Checked THEN
	li_especie  = -1
ELSE
   li_especie 	= dw_sel_especie.Object.espe_codigo[1]
END IF

IF MessageBox("Atención", "Está Seguro que desea Eliminar Información?", Question!, YesNo!) = 1 THEN
			w_main.SetMicroHelp("Borrando Registro...")
						
			DECLARE Elimina_precios_en_valofactprod PROCEDURE FOR dbo.Elimina_precios_en_valofactprod
			@especie = :li_especie;

	      EXECUTE Elimina_precios_en_valofactprod ;
			
			dw_1.reset()
ELSE
  RETURN
END IF


end event

event ue_antesguardar;call super::ue_antesguardar;Traspasa_datos()
TriggerEvent("ue_validaregistro")
			 

end event

event ue_guardar;//
IF dw_1.AcceptText() = -1 THEN RETURN
is_encuentra=""
SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")
Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = 0 THEN
   IF wf_actualiza_db() THEN
		w_main.SetMicroHelp("Información Grabada.")
		MessageBox( "Atención", "El Proceso a Concluído Satisfactoriamente.", &
			Information!, Ok!)
   ELSE
	   w_main.SetMicroHelp("No se puede Grabar información.")
	   Message.DoubleParm = -1
	   RETURN
	END IF
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
   Message.DoubleParm = -1
   RETURN
END IF
end event

event close;//
Boolean	Valida
Window	ventana
Integer	li_vta

ventana	= This.ParentWindow().GetFirstSheet()

IF IsValid(ventana) THEN
	li_vta++

	DO
		ventana	= this.ParentWindow().GetNextSheet(ventana)
		valida	= IsValid(ventana)
		IF valida THEN li_vta++
	LOOP WHILE valida
END IF


GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

event ue_listo;//
String	ls_Objetos, ls_Columna, ls_NumeroTab
Integer	li_Posicion

IF dw_1.RowCount() > 0 THEN
	IF istr_mant.Solo_Consulta THEN
		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_Insertar.Enabled	=	False
		
		wf_BloqueaColumnas(True)
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_Insertar.Enabled	=	False
	ELSE
		pb_Insertar.Enabled	=	True
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

type st_encabe from w_mant_directo`st_encabe within w_carga_precios_fichaproductores
integer width = 2606
integer height = 184
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_carga_precios_fichaproductores
integer x = 2866
integer y = 356
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;Integer  li_null
SetNull(li_null)

dw_sel_especie.SetItem(1,"espe_codigo", li_null)
cbx_especie.Checked = TRUE
pb_grabar.Enabled	  = False
pb_eliminar.Enabled = False
pb_lectura.Enabled  = False
pb_eli_det.Enabled  = False
is_encuentra = ""
pb_lectura.Enabled  = TRUE


end event

type pb_lectura from w_mant_directo`pb_lectura within w_carga_precios_fichaproductores
integer x = 3008
integer y = 164
integer taborder = 30
end type

event pb_lectura::clicked;call super::clicked;pb_lectura.Enabled = FALSE
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_carga_precios_fichaproductores
integer x = 2857
integer y = 712
integer taborder = 60
string picturename = "\Desarrollo 12\Imagenes\Botones\EliminaEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\EliminarDisab.png"
end type

type pb_insertar from w_mant_directo`pb_insertar within w_carga_precios_fichaproductores
boolean visible = false
integer x = 2857
integer y = 532
integer taborder = 0
end type

type pb_salir from w_mant_directo`pb_salir within w_carga_precios_fichaproductores
integer x = 2885
integer y = 1140
end type

event pb_salir::clicked;//
Close (parent)
end event

type pb_imprimir from w_mant_directo`pb_imprimir within w_carga_precios_fichaproductores
boolean visible = false
integer x = 2853
integer y = 1068
integer taborder = 0
end type

type pb_grabar from w_mant_directo`pb_grabar within w_carga_precios_fichaproductores
integer x = 2857
integer y = 892
integer taborder = 70
end type

type dw_1 from w_mant_directo`dw_1 within w_carga_precios_fichaproductores
integer x = 87
integer y = 556
integer width = 2601
integer height = 1124
integer taborder = 50
boolean titlebar = true
string title = "Carga de Datos"
string dataobject = "dw_carga_precios_fichaproductores"
boolean hscrollbar = true
end type

type st_6 from statictext within w_carga_precios_fichaproductores
integer x = 114
integer y = 124
integer width = 475
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
string text = "Fuente de Datos"
boolean focusrectangle = false
end type

type st_12 from statictext within w_carga_precios_fichaproductores
integer x = 114
integer y = 376
integer width = 238
integer height = 68
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type dw_sel_especie from datawindow within w_carga_precios_fichaproductores
integer x = 635
integer y = 352
integer width = 878
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String ls_Nula
SetNull(ls_Nula)

iuo_especie		=	CREATE	uo_especie
IF iuo_especie.existe(Integer(Data),True,sqlca) = False THEN
	This.SetItem(1, "espe_codigo", Long(ls_nula))
	RETURN 1
ELSE
	habilita_lectura()
END IF
Destroy iuo_especie


	   
end event

type ddlb_fuentes from dropdownlistbox within w_carga_precios_fichaproductores
integer x = 635
integer y = 116
integer width = 1120
integer height = 424
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
boolean sorted = false
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;is_base	= This.Text
IF Not ConexionExportaciones() THEN
	MessageBox("Atención","Error de Conexión ",Exclamation!)
	RETURN 
ELSE
habilita_lectura()
END IF


end event

type dw_paso from datawindow within w_carga_precios_fichaproductores
boolean visible = false
integer x = 87
integer y = 556
integer width = 2601
integer height = 860
boolean titlebar = true
string title = "Carga de Datos"
string dataobject = "dw_trae_precios_fichaproductores"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_carga_precios_fichaproductores
integer x = 78
integer y = 252
integer width = 2606
integer height = 232
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_especie from checkbox within w_carga_precios_fichaproductores
integer x = 640
integer y = 276
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todas"
boolean checked = true
end type

event clicked;IF cbx_especie.Checked THEN
	istr_mant.argumento[2]	= "0"
   dw_sel_especie.Enabled	=	False
ELSE
	dw_sel_especie.Enabled	=	True
END IF

end event

type pb_eli_det from picturebutton within w_carga_precios_fichaproductores
boolean visible = false
integer x = 2875
integer y = 1424
integer width = 155
integer height = 132
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\desarrollo\bmp\suprime.bmp"
string disabledname = "\desarrollo\bmp\suprimd.bmp"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent ("ue_borrar_detalle")
end event

