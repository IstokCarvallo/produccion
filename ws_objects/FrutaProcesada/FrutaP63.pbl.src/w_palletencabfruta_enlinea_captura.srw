$PBExportHeader$w_palletencabfruta_enlinea_captura.srw
forward
global type w_palletencabfruta_enlinea_captura from w_para_informes
end type
type dw_1 from datawindow within w_palletencabfruta_enlinea_captura
end type
type dw_2 from datawindow within w_palletencabfruta_enlinea_captura
end type
type st_1 from statictext within w_palletencabfruta_enlinea_captura
end type
type pb_archivo from picturebutton within w_palletencabfruta_enlinea_captura
end type
type gb_3 from groupbox within w_palletencabfruta_enlinea_captura
end type
type st_9 from statictext within w_palletencabfruta_enlinea_captura
end type
end forward

global type w_palletencabfruta_enlinea_captura from w_para_informes
integer x = 14
integer y = 32
integer width = 3781
integer height = 1992
string title = "Captura Archivo Existencia de Linea"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_1 dw_1
dw_2 dw_2
st_1 st_1
pb_archivo pb_archivo
gb_3 gb_3
st_9 st_9
end type
global w_palletencabfruta_enlinea_captura w_palletencabfruta_enlinea_captura

type variables
str_busqueda 	istr_busq
str_mant 		istr_mant

String is_control_d
Integer ii_control, ii_calificacion

DataWindowChild	idwc_cliente, idwc_especie, idwc_stat, idwc_categorias

uo_seleccion_especie					iuo_selespecie
uo_seleccion_variedad				iuo_selvariedad
uo_seleccion_varios_productores	iuo_selproductor

end variables

forward prototypes
public function boolean existe_pallet ()
public function boolean wf_actualiza_db ()
end prototypes

public function boolean existe_pallet ();Long		ll_cont, ll_numero, ll_productor, ll_secuencia, ll_fila, ll_prueba
Integer	li_cliente, li_planta, li_especie, li_variedad, li_condicion, li_etiqueta
String	ls_embalaje, ls_calibre
DateTime	ldt_hora

FOR ll_fila = 1 TO dw_1.RowCount()
	
	li_cliente 	= dw_1.Object.clie_codigo[ll_fila]
	li_planta	= dw_1.Object.plde_codigo[ll_fila]
	ll_numero	= dw_1.Object.paen_numero[ll_fila]
	li_especie	= dw_1.Object.espe_codigo[ll_fila]	
	li_variedad	= dw_1.Object.vari_codigo[ll_fila]
	ls_embalaje	= dw_1.Object.emba_codigo[ll_fila]
	ll_productor= dw_1.Object.prod_codigo[ll_fila]
	li_condicion= dw_1.Object.cond_codigo[ll_fila]
	li_etiqueta	= dw_1.Object.etiq_codigo[ll_fila]
	ll_secuencia= dw_1.Object.pafr_secuen[ll_fila]
	ldt_hora		= dw_1.Object.pafr_horcar[ll_fila]
	ls_calibre	= dw_1.Object.pafr_calibr[ll_fila]

	SELECT count()
	INTO :ll_cont
	FROM dba.palletencabfruta_enlinea
	WHERE clie_codigo = :li_cliente
	  AND plde_codigo = :li_planta
	  AND paen_numero = :ll_numero
	  AND espe_codigo = :li_especie
	  AND vari_codigo = :li_variedad
	  AND emba_codigo = :ls_embalaje
	  AND prod_codigo = :ll_productor
	  AND cond_codigo = :li_condicion
	  AND etiq_codigo = :li_etiqueta
	  AND pafr_calibr = :ls_calibre
	  AND pafr_secuen = :ll_secuencia
	  AND pafr_horcar = :ldt_hora;
	  
	  st_1.Text = 'Cargando'
	  
	  IF ll_cont = 0 THEN
			dw_1.Rowscopy(ll_fila, ll_fila, Primary!,dw_2,1, Primary!)
		ELSE
			ll_prueba = 1
	  END IF	
	  
NEXT	  
  
 RETURN TRUE
end function

public function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_2.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_2.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

dw_1.Reset()
dw_2.Reset()

st_1.Text = 'Carga Realizada'

RETURN lb_Retorno
end function

on w_palletencabfruta_enlinea_captura.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.dw_2=create dw_2
this.st_1=create st_1
this.pb_archivo=create pb_archivo
this.gb_3=create gb_3
this.st_9=create st_9
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.dw_2
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.pb_archivo
this.Control[iCurrent+5]=this.gb_3
this.Control[iCurrent+6]=this.st_9
end on

on w_palletencabfruta_enlinea_captura.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.st_1)
destroy(this.pb_archivo)
destroy(this.gb_3)
destroy(this.st_9)
end on

event open;call super::open;Boolean lb_Cerrar

dw_1.SetTransObject(Sqlca)
dw_2.SetTransObject(Sqlca)


end event

event resize;//pb_acepta.x			=	This.WorkSpaceWidth() - 292
//pb_acepta.y			=	gb_1.y + 88
//pb_acepta.width	=	156
//pb_acepta.height	=	133
end event

type st_computador from w_para_informes`st_computador within w_palletencabfruta_enlinea_captura
integer x = 2363
end type

type st_usuario from w_para_informes`st_usuario within w_palletencabfruta_enlinea_captura
integer x = 2363
end type

type st_temporada from w_para_informes`st_temporada within w_palletencabfruta_enlinea_captura
integer x = 2363
end type

type p_logo from w_para_informes`p_logo within w_palletencabfruta_enlinea_captura
end type

type st_titulo from w_para_informes`st_titulo within w_palletencabfruta_enlinea_captura
integer width = 3040
string text = "Captura Archivo Existencia de Linea"
end type

type pb_acepta from w_para_informes`pb_acepta within w_palletencabfruta_enlinea_captura
string tag = "Imprimir Reporte"
integer x = 3438
integer y = 1108
integer taborder = 90
string picturename = "\Desarrollo 12\Imagenes\Botones\GrabaEnab.png"
string disabledname = "\Desarrollo 12\Imagenes\Botones\GrabaDisab.png"
string powertiptext = "Imprimir Reporte"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

existe_pallet()


wf_actualiza_db()
end event

type pb_salir from w_para_informes`pb_salir within w_palletencabfruta_enlinea_captura
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 3438
integer y = 1372
integer taborder = 100
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_1 from datawindow within w_palletencabfruta_enlinea_captura
integer x = 288
integer y = 492
integer width = 2962
integer height = 1148
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletencabfruta_enlinea"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_palletencabfruta_enlinea_captura
boolean visible = false
integer x = 64
integer y = 1948
integer width = 686
integer height = 400
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_palletencabfruta_enlinea"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_palletencabfruta_enlinea_captura
integer x = 251
integer y = 1684
integer width = 3040
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type pb_archivo from picturebutton within w_palletencabfruta_enlinea_captura
integer x = 3438
integer y = 840
integer width = 233
integer height = 196
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 12\Imagenes\Botones\BuscaArch.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;String	ls_directorio, ls_archivo, path, nombre, pathpordefault
Integer	li_valida, li_opcion = 1
Long		ll_rc, ll_fila

dw_1.Reset()

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, pathpordefault)

IF GetFileOpenName ("Carga de Archivo", path, nombre,  "csv", "Archivos Planos (*.csv),*.csv", pathpordefault)< 1 THEN Return

li_valida = dw_1.importfile(path)

IF li_valida = 0 THEN
	pb_salir.SetFocus()
	dw_1.Insertrow(0)
	RETURN
ELSEIF li_valida = -1 THEN
	MessageBox("Error de Apertura","Ocurrió un error al utilizar el archivo",Information!,Ok!)
	Message.DoubleParm = 1
ELSE				
	Message.DoubleParm = 2
		
	IF dw_1.RowCount() = 0 THEN 
		MessageBox("Atención", "No se cargó archivo exitosamente.", StopSign!, Ok!)
		FileClose(li_valida)
	ELSE			
		FileClose(li_valida)
		
		st_1.Text = 'Grabe Datos'
		
	END IF
		
END IF




end event

type gb_3 from groupbox within w_palletencabfruta_enlinea_captura
integer x = 3383
integer y = 752
integer width = 270
integer height = 272
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
end type

type st_9 from statictext within w_palletencabfruta_enlinea_captura
integer x = 251
integer y = 440
integer width = 3040
integer height = 1208
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

