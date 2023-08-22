$PBExportHeader$w_archivo_tablas_alhue.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_archivo_tablas_alhue from w_para_informes
end type
type em_fdesde from editmask within w_archivo_tablas_alhue
end type
type em_fhasta from editmask within w_archivo_tablas_alhue
end type
type st_14 from statictext within w_archivo_tablas_alhue
end type
type st_12 from statictext within w_archivo_tablas_alhue
end type
type cbx_2 from checkbox within w_archivo_tablas_alhue
end type
type gb_6 from groupbox within w_archivo_tablas_alhue
end type
type st_1 from statictext within w_archivo_tablas_alhue
end type
type sle_mensa from statictext within w_archivo_tablas_alhue
end type
type st_5 from statictext within w_archivo_tablas_alhue
end type
type dw_1 from datawindow within w_archivo_tablas_alhue
end type
type dw_2 from datawindow within w_archivo_tablas_alhue
end type
end forward

global type w_archivo_tablas_alhue from w_para_informes
integer width = 2917
integer height = 1296
string title = "ARCHIVOS MOVIMIENTOS GENERADOS"
boolean maxbox = false
boolean resizable = false
em_fdesde em_fdesde
em_fhasta em_fhasta
st_14 st_14
st_12 st_12
cbx_2 cbx_2
gb_6 gb_6
st_1 st_1
sle_mensa sle_mensa
st_5 st_5
dw_1 dw_1
dw_2 dw_2
end type
global w_archivo_tablas_alhue w_archivo_tablas_alhue

type variables
str_mant istr_mant
DataWindowChild 		idwc_cliente, idwc_planta, idwc_transporte					

Integer	ii_Cliente, ii_Planta, ii_transporte
String	is_NomPlanta, is_NomCliente


end variables

forward prototypes
public function long buscaproceso ()
end prototypes

public function long buscaproceso ();Long	ll_proceso

SELECT max(cont_proces)
INTO :ll_proceso
FROM DBA.ControlContenedor;

IF ll_proceso = 0 THEN
	ll_proceso = 1
END IF	

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla ControlContenedor")
END IF

Return ll_proceso


end function

on w_archivo_tablas_alhue.create
int iCurrent
call super::create
this.em_fdesde=create em_fdesde
this.em_fhasta=create em_fhasta
this.st_14=create st_14
this.st_12=create st_12
this.cbx_2=create cbx_2
this.gb_6=create gb_6
this.st_1=create st_1
this.sle_mensa=create sle_mensa
this.st_5=create st_5
this.dw_1=create dw_1
this.dw_2=create dw_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_fdesde
this.Control[iCurrent+2]=this.em_fhasta
this.Control[iCurrent+3]=this.st_14
this.Control[iCurrent+4]=this.st_12
this.Control[iCurrent+5]=this.cbx_2
this.Control[iCurrent+6]=this.gb_6
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.sle_mensa
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.dw_1
this.Control[iCurrent+11]=this.dw_2
end on

on w_archivo_tablas_alhue.destroy
call super::destroy
destroy(this.em_fdesde)
destroy(this.em_fhasta)
destroy(this.st_14)
destroy(this.st_12)
destroy(this.cbx_2)
destroy(this.gb_6)
destroy(this.st_1)
destroy(this.sle_mensa)
destroy(this.st_5)
destroy(this.dw_1)
destroy(this.dw_2)
end on

event open;call super::open;x				= 0
y				= 0
//This.Height	= 2020

//gi_CodPlanta

em_fdesde.text					=	String(RelativeDate(Today() , -365))
em_fhasta.text					=	String(Today())	


end event

type st_computador from w_para_informes`st_computador within w_archivo_tablas_alhue
end type

type st_usuario from w_para_informes`st_usuario within w_archivo_tablas_alhue
end type

type st_temporada from w_para_informes`st_temporada within w_archivo_tablas_alhue
end type

type p_logo from w_para_informes`p_logo within w_archivo_tablas_alhue
end type

type st_titulo from w_para_informes`st_titulo within w_archivo_tablas_alhue
integer width = 2171
integer height = 104
string text = "Genera Tablas Movimientos Alhue"
end type

type pb_acepta from w_para_informes`pb_acepta within w_archivo_tablas_alhue
integer x = 2629
integer y = 408
integer taborder = 160
string picturename = "\Desarrollo 12\Imagenes\Botones\Excel.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\Excel_bn.png"
end type

event pb_acepta::clicked;Long	  	ll_Fila, ll_Fila1
Date		ld_fdesde, ld_fhasta
String	ls_Archivo, ls_Registro, ls_ruta, ls_hora, ls_planta

SetPointer(HourGlass!)

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

ls_planta = string(gi_CodPlanta,'000')

IF cbx_2.Checked THEN
	ld_fdesde = Date(19000101)
	ld_fhasta = Date(Today())
ELSE
	ld_fdesde = Date(em_fdesde.Text)
	ld_fhasta = Date(em_fhasta.Text)
END IF	

ll_Fila	=	dw_1.Retrieve(ld_fdesde,ld_fhasta)

IF ll_Fila = -1 THEN
	F_ErrorBaseDatos(sqlca,"Generacion Tablas de Movimientos.")
	sle_mensa.Text = 'Error En Recuperación'
	dw_1.Reset()
	dw_2.Reset()
	commit;						
	Return
ELSEIF ll_Fila = 0 THEN
	MessageBox("Atención", "No hay información Para Tablas de Movimientos.", &
					Exclamation!, Ok!)
	sle_mensa.Text = 'No Existe Información'
	dw_1.Reset()
	dw_2.Reset()
	commit;						
	Return
ELSE
	
	ls_hora = String(Time(now()),'hhmm')
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	ls_Archivo	= '\palletencab'+ls_planta+String(today(),'yyyymmdd')+ls_hora+'.csv'
	
	IF dw_1.SaveAs(ls_ruta + ls_archivo, CSV!	 ,false) = -1 THEN
		MessageBox("Error", "No se logró Almacenar el archivo generado en la carpeta especificada~n~r" + &
						ls_ruta + ls_archivo+"~n~r", StopSign!)
		sle_mensa.Text = 'NO se Generó Archivo PalletEncab'
		dw_1.Reset()
		dw_2.Reset()
		commit;						
		Return
	ELSE
		sle_mensa.Text = 'Archivo Palletencab Generado en Mis Documentos.'
		dw_1.Reset()
		commit;
	END IF	
END IF

ll_Fila1	=	dw_2.Retrieve(ld_fdesde,ld_fhasta)

IF ll_Fila1 = -1 THEN
	F_ErrorBaseDatos(sqlca,"Generacion Tablas de Movimientos.")
	sle_mensa.Text = 'Error En Recuperación'
	dw_2.Reset()
	commit;				
	Return
ELSEIF ll_Fila1 = 0 THEN
	MessageBox("Atención", "No hay información Para Tablas de Movimientos.", &
					Exclamation!, Ok!)
	sle_mensa.Text = 'No Existe Información'
	dw_2.Reset()
	commit;						
	Return
ELSE
		
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
	ls_Archivo	= '\palletfruta'+ls_planta+String(today(),'yyyymmdd')+ls_hora+'.csv'
	
	IF dw_2.SaveAs(ls_ruta + ls_archivo, CSV!	 ,false) = -1 THEN
		MessageBox("Error", "No se logró Almacenar el archivo generado en la carpeta especificada~n~r" + &
						ls_ruta + ls_archivo+"~n~r", StopSign!)
		sle_mensa.Text = 'NO se Generó Archivo Palletfruta en Mis Documentos'
		dw_1.Reset()
		dw_2.Reset()
		commit;						
		Return
	ELSE
		sle_mensa.Text = 'Archivo Palletencab Generado en Mis Documentos.'
		dw_1.Reset()
		dw_2.Reset()
		commit;
		Return
	END IF	
END IF

sle_mensa.Text = 'Ambos Archivos Generados en Mis Documentos'
dw_1.Reset()
dw_2.Reset()
commit;
Return
end event

type pb_salir from w_para_informes`pb_salir within w_archivo_tablas_alhue
integer x = 2629
integer y = 748
integer taborder = 170
end type

type em_fdesde from editmask within w_archivo_tablas_alhue
integer x = 905
integer y = 584
integer width = 402
integer height = 92
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type em_fhasta from editmask within w_archivo_tablas_alhue
integer x = 1618
integer y = 584
integer width = 402
integer height = 92
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_14 from statictext within w_archivo_tablas_alhue
integer x = 1376
integer y = 608
integer width = 206
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Hasta"
boolean focusrectangle = false
end type

type st_12 from statictext within w_archivo_tablas_alhue
integer x = 585
integer y = 608
integer width = 256
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Desde"
boolean focusrectangle = false
end type

type cbx_2 from checkbox within w_archivo_tablas_alhue
integer x = 2066
integer y = 588
integer width = 311
integer height = 76
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;
IF This.Checked THEN
	em_fdesde.Text = '19000101'
	em_fhasta.Text = String(Today())
	em_fdesde.Enabled = False
	em_fhasta.Enabled = False

ELSE
	em_fdesde.Enabled = True
	em_fhasta.Enabled = True
	em_fhasta.text					=	String(RelativeDate(Today() , -365))
	em_fhasta.text					=	String(Today())	
	em_fdesde.SetFocus()
END IF
end event

type gb_6 from groupbox within w_archivo_tablas_alhue
integer x = 256
integer y = 508
integer width = 2139
integer height = 196
integer taborder = 180
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Fecha Ultimo Movimiento"
end type

type st_1 from statictext within w_archivo_tablas_alhue
integer x = 247
integer y = 444
integer width = 2171
integer height = 436
integer textsize = -10
integer weight = 400
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

type sle_mensa from statictext within w_archivo_tablas_alhue
integer x = 274
integer y = 904
integer width = 2117
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_5 from statictext within w_archivo_tablas_alhue
integer x = 247
integer y = 880
integer width = 2171
integer height = 164
integer textsize = -10
integer weight = 400
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

type dw_1 from datawindow within w_archivo_tablas_alhue
boolean visible = false
integer x = 311
integer y = 1408
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_genera_tabla_palletencab"
boolean border = false
boolean livescroll = true
end type

type dw_2 from datawindow within w_archivo_tablas_alhue
integer x = 1454
integer y = 1576
integer width = 686
integer height = 400
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_genera_tabla_palletfruta"
boolean border = false
boolean livescroll = true
end type

