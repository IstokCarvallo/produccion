$PBExportHeader$w_proc_traspaso_zonas.srw
$PBExportComments$Ventana para Consolidar datos de las Zonas.
forward
global type w_proc_traspaso_zonas from window
end type
type dw_2 from datawindow within w_proc_traspaso_zonas
end type
type dw_1 from datawindow within w_proc_traspaso_zonas
end type
type dw_errores from datawindow within w_proc_traspaso_zonas
end type
type st_2 from statictext within w_proc_traspaso_zonas
end type
type ddlb_bases from dropdownlistbox within w_proc_traspaso_zonas
end type
type sle_mensa from singlelineedit within w_proc_traspaso_zonas
end type
type st_5 from statictext within w_proc_traspaso_zonas
end type
type st_1 from statictext within w_proc_traspaso_zonas
end type
type pb_salir from picturebutton within w_proc_traspaso_zonas
end type
type pb_grabar from picturebutton within w_proc_traspaso_zonas
end type
type gb_2 from groupbox within w_proc_traspaso_zonas
end type
type gb_1 from groupbox within w_proc_traspaso_zonas
end type
type st_6 from statictext within w_proc_traspaso_zonas
end type
end forward

global type w_proc_traspaso_zonas from window
integer width = 3584
integer height = 2092
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 12632256
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
dw_2 dw_2
dw_1 dw_1
dw_errores dw_errores
st_2 st_2
ddlb_bases ddlb_bases
sle_mensa sle_mensa
st_5 st_5
st_1 st_1
pb_salir pb_salir
pb_grabar pb_grabar
gb_2 gb_2
gb_1 gb_1
st_6 st_6
end type
global w_proc_traspaso_zonas w_proc_traspaso_zonas

type variables
Boolean		ib_Conectado
Integer		ii_fuente
Date		id_FechaAcceso
Time		it_HoraAcceso



Transaction	itr_Fuente
end variables

forward prototypes
public function boolean wf_actualiza_db ()
public function boolean conexionfuente (string as_base)
public subroutine actualizafechas ()
public function boolean traspaso (string as_dataobject, string as_descripcion)
end prototypes

event ue_guardar();String	ls_dataobject, ls_descripcion

SetPointer(HourGlass!)

/*
Traspaso de tabla PalletEncab.
*/

ls_DataObject	=	"ppl_palletencab"
ls_descripcion	=	"Encabezado de Pallets [PalletEncab]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla PalletFruta.
*/

ls_DataObject	=	"ppl_palletfruta"
ls_descripcion	=	"Detalle de Pallets [PalletFruta]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla PalletBitacora
*/

ls_DataObject	=	"ppl_palletbitacora"
ls_descripcion	=	"Bitacora del Pallet [PalletBitacora]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla RecFruProcee.
*/

ls_DataObject	=	"ppl_recfruprocee"
ls_descripcion	=	"Recepción de Fruta [RecFruProcee]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla RecFruProppe.
*/

ls_DataObject	=	"ppl_recfruproppe"
ls_descripcion	=	"Recepción de Fruta [RecFruProppe]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla RecFruProppd.
*/

ls_DataObject	=	"ppl_recfruproppd"
ls_descripcion	=	"Recepción de Fruta [RecFruProppd]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla Embarqueprod.
*/

ls_DataObject	=	"ppl_embarqueprod"
ls_descripcion	=	"Embarques de Producción [EmbarqueProd]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla DespaFrigoEn.
*/

ls_DataObject	=	"ppl_despafrigoen"
ls_descripcion	=	"Encabezado de Despacho [DespaFrigoen]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla RepalletEnca.
*/

ls_DataObject	=	"ppl_repalletenca"
ls_descripcion	=	"Encabezado de Repalletizado [RepalletEnca]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla RepalletDeta.
*/

ls_DataObject	=	"ppl_repalletdeta"
ls_descripcion	=	"Detalle de Repalletizado [RepalletDeta]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla InspecPalEnc.
*/

ls_DataObject	=	"ppl_inspecpalenc"
ls_descripcion	=	"Encabezado de Fruta Inspeccionada [InsPecPalEnc]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla InsPecPalDet.
*/

ls_DataObject	=	"ppl_inspecpaldet"
ls_descripcion	=	"Detalle de Fruta Inspeccionada [InsPecPalDet]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla MovFruSinProEn.
*/

ls_DataObject	=	"ppl_movfrusinproen"
ls_descripcion	=	"Movimiento de Fruta [MovFruSinProEn]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla MovFruSinProDe.
*/

ls_DataObject	=	"ppl_movfrusinprode"
ls_descripcion	=	"Movimiento de Fruta [MovFruSinProDe]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla InforProduc.
*/

ls_DataObject	=	"ppl_inforproduc"
ls_descripcion	=	"Informe de Producción [InforProDuc]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla HistContCalidad.
*/

ls_DataObject	=	"ppl_histcontcalidad"
ls_descripcion	=	"Histórico Control Calidad [HistContCalidad]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla PalletEmbar.
*/

ls_DataObject	=	"ppl_palletembar"
ls_descripcion	=	"Pallet Embarcados [PalletEmbar]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla DespaFrigode.
*/

ls_DataObject	=	"ppl_despafrigode"
ls_descripcion	=	"Despacho de Fruta [DespaFrigode]"

IF NOT Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

/*
Traspaso de tabla RecFruProced.
*/

ls_DataObject	=	"ppl_recfruproced"
ls_descripcion	=	"Recepción de Fruta [RecFruProced]"

IF Not Traspaso(ls_DataObject, ls_Descripcion) THEN RETURN

ActualizaFechas()

SetPointer(Arrow!)

MessageBox("Atención","El Traspaso ha finalizado satisfactoriamente.", Exclamation!, Ok!)
end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean wf_actualiza_db ();IF dw_1.Update() = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		RETURN False
	ELSE
		RETURN True
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	RETURN False
END IF

RETURN True
end function

public function boolean conexionfuente (string as_base);String			ls_server1 = "12345678", ls_nombre
Integer			li_posic, li_resp
Boolean			lb_Retorno	=	True
itr_fuente	=	CREATE Transaction


IF ib_Conectado THEN
	DISCONNECT Using itr_Fuente ;
END IF

itr_Fuente.SQLCode	= 1
ls_server1				= ProfileString(gstr_apl.ini, as_Base, "servername", "12345678")

IF ls_server1 <> "12345678" THEN
	ls_nombre					= ProfileString(gstr_apl.ini, as_Base, "NombreOdbc", "")
	itr_Fuente.Dbms			= lower(ProFileString(gstr_apl.ini, as_Base, "dbms", "ODBC"))
	itr_Fuente.ServerName	= ProfileString(gstr_apl.ini, as_Base, "servername", "")
	itr_Fuente.DataBase		= ProFileString(gstr_apl.ini, as_Base, "database", "")
	itr_Fuente.DbParm			= "Connectstring='DSN=" + ls_nombre + ";UID=" + sqlca.LogId + &
										";PWD=" + sqlca.LogPass + "'// ;PBUseProcOwner = " + '"Yes"'
ELSE
	MessageBox(This.Title,"No se puede ejecutar la aplicación por la falta de archivo " &
					+ gstr_apl.ini + ". Verifique que el archivo exista y que esté en el directorio " + &
					"donde la aplicación esté corriendo o en el PATH del Computador del cliente.",StopSign!)
	lb_Retorno		=	False
END IF

CONNECT Using itr_Fuente ; 

IF itr_Fuente.SQLCode <> 0 THEN
	IF itr_Fuente.SQLDBCode = -103 THEN
		MessageBox("Error de Conexión", "Usuario o Password con que se ingresó~r" + &
						"no tiene acceso a Fuente seleccionada.~r~r" + &
						"Seleccione otra Fuente o consulte al Administrador de Sistemas.", &
						Information!, Ok!)
	ELSEIF itr_Fuente.SQLDBCode = -102 THEN
		MessageBox("Error de Conexión", "Demasiados Usuarios conectados a la Base.~r~r" + &
						"Consulte con Administrador", StopSign!, Ok!)
	ELSEIF itr_Fuente.SQLDBCode <> 0 THEN
		F_ErrorBaseDatos(itr_Fuente, This.Title)
	ELSE
		ib_Conectado	=	True
		lb_Retorno		=	False
	END IF
END IF

RETURN lb_Retorno
end function

public subroutine actualizafechas ();Long		ll_fila, ll_FilaFecha, ll_NuevaFila
Integer	li_Cliente, li_Planta

dw_1.SetTransObject(sqlca)
dw_1.Retrieve()

dw_2.SetTransObject(itr_Fuente)
dw_2.Retrieve()

FOR ll_fila = 1 TO dw_2.RowCount()
	li_Cliente		=	dw_2.Object.clie_codigo[ll_fila]
	li_Planta		=	dw_2.Object.plde_codigo[ll_fila]
	ll_FilaFecha	=	dw_1.Find("clie_codigo = " + String(li_Cliente) + " and " + &
							"plde_codigo = " + String(li_Planta), 1, dw_1.RowCount())
	
	IF ll_FilaFecha > 0 THEN
		dw_1.Object.fapl_fechac[ll_FilaFecha]	=	Today()
		dw_1.Object.fapl_horact[ll_FilaFecha]	=	Now()
		dw_1.Object.fapl_descri[ll_FilaFecha]	=	String(Today(),'dd/mm/yyyy') + '  ' + &
																String(Now())
	ELSE
		ll_NuevaFila = dw_1.InsertRow(0)															
		dw_1.Object.clie_codigo[ll_NuevaFila]	=	String(li_cliente)
		dw_1.Object.plde_codigo[ll_NuevaFila]	=	String(li_planta)
		dw_1.Object.fapl_fechac[ll_NuevaFila]	=	Today()
		dw_1.Object.fapl_horact[ll_NuevaFila]	=	Now()
		dw_1.Object.fapl_descri[ll_NuevaFila]	=	String(Today(),'dd/mm/yyyy') + '  ' + &
																String(Now())
	END IF
NEXT

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
END IF

end subroutine

public function boolean traspaso (string as_dataobject, string as_descripcion);Pipeline	lpl_Traspaso
Boolean	lb_Retorno	= True
Integer	li_Retorno
String	ls_Errores[] = {"Falló Apertura de Traspaso", &
								"Demasiadas Columnas", &
								"Tabla ya Existe", &
								"Tabla no Existe", &
								"Perdió Conexión", &
								"Argumentos Erróneos", &
								"Columna(s) de distinto Tipo", &
								"Error Fatal en SQL de Fuente", &
								"Error Fatal en SQL de Destino", &
								"Exedió máximo de Errores", &
								"Error de Sintáxis en Tabla", &
								"Tabla requiere de LLave pero no tiene", &
								"Traspaso ya estaba en Progreso", &
								"Error en Base de Datos Fuente", &
								"Error en Base de Datos Destino", &
								"Base de Datos Destino es de Sólo Lectura"}

lpl_Traspaso	=	CREATE Pipeline

lpl_Traspaso.DataObject	=	as_dataobject

sle_mensa.Text	=	"Traspasando de tabla " + as_descripcion

li_Retorno	=	lpl_Traspaso.Start(itr_Fuente, sqlca, dw_errores)

IF li_Retorno < 0 THEN
	MessageBox("Error", "Se ha producido el siguiente Error en el Traspaso de " + &
					as_descripcion + ". ~r~r" + String(li_Retorno) + " : " + &
					ls_Errores[Abs(li_Retorno)] + "~r~rAvise a Administrador de Sistema.")
	lb_retorno	=	False
ELSE
	IF dw_errores.RowCount() > 0 THEN
		IF MessageBox("Atención", "Se han Producido Errores en Traspaso de Datos~r~r" + &
							"Desea Continuar ?", Question!, YesNo!, 2) = 2 THEN 
			lb_retorno	=	False
		ELSE
			lb_retorno	=	True
		END IF
	END IF
END IF

Destroy lpl_Traspaso;

RETURN	lb_retorno
end function

on w_proc_traspaso_zonas.create
this.dw_2=create dw_2
this.dw_1=create dw_1
this.dw_errores=create dw_errores
this.st_2=create st_2
this.ddlb_bases=create ddlb_bases
this.sle_mensa=create sle_mensa
this.st_5=create st_5
this.st_1=create st_1
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.gb_2=create gb_2
this.gb_1=create gb_1
this.st_6=create st_6
this.Control[]={this.dw_2,&
this.dw_1,&
this.dw_errores,&
this.st_2,&
this.ddlb_bases,&
this.sle_mensa,&
this.st_5,&
this.st_1,&
this.pb_salir,&
this.pb_grabar,&
this.gb_2,&
this.gb_1,&
this.st_6}
end on

on w_proc_traspaso_zonas.destroy
destroy(this.dw_2)
destroy(this.dw_1)
destroy(this.dw_errores)
destroy(this.st_2)
destroy(this.ddlb_bases)
destroy(this.sle_mensa)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.st_6)
end on

event open;String	ls_linea
Integer	li_archivo, li_inicio, li_termino

x				= 0
y				= 0
This.Icon	=	Gstr_apl.Icono

li_archivo				=	FileOpen(gstr_apl.Ini)

IF li_archivo < 0 THEN
	MessageBox("Error","Archivo " + gstr_apl.ini + " no se encuentra en directorio.", StopSign!)
ELSE
	SetPointer(HourGlass!)
	
	ddlb_bases.SetRedraw(False)
	ddlb_bases.Reset()

	DO WHILE FileRead(li_archivo,ls_linea) >= 0
		li_inicio	= Pos(ls_linea,"[",1)
		li_termino	= Pos(ls_linea,"]",1)
		
		IF li_inicio > 0 AND li_termino>0 THEN
			ddlb_bases.AddItem(Mid(ls_linea, li_inicio + 1, li_termino - li_inicio - 1))
		END IF
	LOOP

	FileClose(li_archivo)
	ddlb_bases.SetRedraw(True)
END IF

SetPointer(Arrow!)

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)

end event

event closequery;IF ib_Conectado THEN
	DISCONNECT Using itr_Fuente ;
END IF


GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type dw_2 from datawindow within w_proc_traspaso_zonas
boolean visible = false
integer x = 2661
integer y = 400
integer width = 480
integer height = 172
integer taborder = 70
string title = "none"
string dataobject = "dw_mues_palletfruta_traspaso"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from datawindow within w_proc_traspaso_zonas
boolean visible = false
integer x = 2665
integer y = 592
integer width = 480
integer height = 156
integer taborder = 70
string title = "none"
string dataobject = "dw_mues_fechasactuptas"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_errores from datawindow within w_proc_traspaso_zonas
integer x = 82
integer y = 856
integer width = 3424
integer height = 1104
integer taborder = 80
boolean bringtotop = true
string title = "none"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_proc_traspaso_zonas
integer x = 306
integer y = 296
integer width = 485
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Fuente de Datos"
boolean focusrectangle = false
end type

type ddlb_bases from dropdownlistbox within w_proc_traspaso_zonas
integer x = 306
integer y = 392
integer width = 1673
integer height = 448
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 8388608
long backcolor = 16777215
boolean sorted = false
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;ii_fuente	=	index
end event

type sle_mensa from singlelineedit within w_proc_traspaso_zonas
integer x = 178
integer y = 688
integer width = 1943
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long backcolor = 12632256
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_proc_traspaso_zonas
integer x = 78
integer y = 68
integer width = 2126
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Proceso de Consolidación desde Zonas"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_1 from statictext within w_proc_traspaso_zonas
integer x = 82
integer y = 224
integer width = 2126
integer height = 396
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_proc_traspaso_zonas
integer x = 2363
integer y = 532
integer width = 155
integer height = 132
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_proc_traspaso_zonas
integer x = 2363
integer y = 276
integer width = 155
integer height = 132
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\desarrollo\bmp\disksave.bmp"
string disabledname = "\desarrollo\bmp\disksavd.bmp"
alignment htextalign = left!
end type

event clicked;String	ls_fuente

IF ii_fuente = 0 THEN
	MessageBox("Atención","Debe Seleccionar la fuente de datos previamente.")
	RETURN
END IF

ls_fuente	=	ddlb_bases.Text(ii_fuente)

IF ConexionFuente(ls_fuente) = False THEN
	ddlb_bases.SetFocus()
	RETURN
ELSE
	Parent.TriggerEvent("ue_guardar")
END IF


end event

type gb_2 from groupbox within w_proc_traspaso_zonas
integer x = 2304
integer y = 192
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type gb_1 from groupbox within w_proc_traspaso_zonas
integer x = 2304
integer y = 452
integer width = 274
integer height = 268
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
end type

type st_6 from statictext within w_proc_traspaso_zonas
integer x = 82
integer y = 620
integer width = 2126
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

