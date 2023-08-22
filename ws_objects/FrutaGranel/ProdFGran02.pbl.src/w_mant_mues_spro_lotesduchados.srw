$PBExportHeader$w_mant_mues_spro_lotesduchados.srw
forward
global type w_mant_mues_spro_lotesduchados from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_spro_lotesduchados
end type
type dw_2 from datawindow within w_mant_mues_spro_lotesduchados
end type
type dw_3 from datawindow within w_mant_mues_spro_lotesduchados
end type
end forward

global type w_mant_mues_spro_lotesduchados from w_mant_tabla
integer width = 3278
integer height = 2056
string title = "ASIGNA RECEPCIONES A DUCHAS"
st_1 st_1
dw_2 dw_2
dw_3 dw_3
end type
global w_mant_mues_spro_lotesduchados w_mant_mues_spro_lotesduchados

type variables
w_mant_deta_spro_duchacontrol iw_mantencion

Integer	ii_planta, ii_tipomovto, ii_tipocontrol

uo_spro_movtofrutagranenca	iuo_spro_movtofrutagranenca
uo_duchacontrol				iuo_duchacontrol
end variables

forward prototypes
public subroutine habilitaencab (boolean habilita)
public subroutine buscaducha ()
public subroutine buscarecepcion ()
public subroutine chequeatipocontrol ()
public function boolean existeducha (string as_columna, string as_valor)
end prototypes

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	dw_3.Object.duch_codigo.Protect = 0
	dw_3.Object.duch_codigo.BackGround.Color = rgb(255,255,255)
	dw_3.Object.codu_nropos.Protect = 0
	dw_3.Object.codu_nropos.BackGround.Color = rgb(255,255,255)
	dw_3.Object.codu_fecini.Protect = 0
	dw_3.Object.codu_fecini.BackGround.Color = rgb(255,255,255)
	dw_3.Object.codu_horini.Protect = 0
	dw_3.Object.codu_horini.BackGround.Color = rgb(255,255,255)
	dw_3.Object.guia_recepc.Protect = 0
	dw_3.Object.guia_recepc.BackGround.Color = rgb(255,255,255)
	dw_3.Object.BuscaDucha.Visible = TRUE
	dw_3.Object.BuscaRecepcion.Visible = TRUE	
ELSE
	dw_3.Object.duch_codigo.Protect = 1
	dw_3.Object.duch_codigo.BackGround.Color = rgb(192,192,192)
	dw_3.Object.codu_nropos.Protect = 1
	dw_3.Object.codu_nropos.BackGround.Color = rgb(192,192,192)
	dw_3.Object.codu_fecini.Protect = 1
	dw_3.Object.codu_fecini.BackGround.Color = rgb(192,192,192)
	dw_3.Object.codu_horini.Protect = 1
	dw_3.Object.codu_horini.BackGround.Color = rgb(192,192,192)
	dw_3.Object.guia_recepc.Protect = 1
	dw_3.Object.guia_recepc.BackGround.Color = rgb(192,192,192)
	dw_3.Object.BuscaDucha.Visible = FALSE
	dw_3.Object.BuscaRecepcion.Visible = FALSE	
END IF
end subroutine

public subroutine buscaducha ();Str_Busqueda	lstr_busq
Integer	li_Ducha, li_Estanque
Date		ld_Fecha
Time		lt_Hora

OpenWithParm(w_busc_duchacontrol, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	istr_mant.argumento[1]	=	lstr_busq.argum[1]
	istr_mant.argumento[2]	=	lstr_busq.argum[2]
	istr_mant.argumento[3]	=	lstr_busq.argum[3]
	istr_mant.argumento[4]	=	lstr_busq.argum[4]
	li_Ducha						=	Integer(lstr_busq.Argum[1])
	li_Estanque					=	Integer(lstr_busq.Argum[2])
	ld_Fecha					=	Date(lstr_busq.Argum[3])										
	lt_Hora						=	Time(lstr_busq.Argum[4])

	IF iuo_duchacontrol.Existe(SqlCa,li_Ducha,li_Estanque,ld_Fecha,lt_Hora,False) THEN
		ChequeaTipoControl()
		dw_3.Object.duch_codigo[1]	=	li_Ducha
		dw_3.Object.codu_nropos[1]	=	li_Estanque
		dw_3.Object.codu_fecini[1]	=	ld_Fecha
		dw_3.Object.codu_horini[1]	=	lt_Hora
	END IF
END IF
end subroutine

public subroutine buscarecepcion ();Str_Busqueda	lstr_busq
Date				ld_FechaInicio

ld_FechaInicio	=	dw_3.Object.codu_fecini[1]

IF Isnull(ld_FechaInicio) THEN ld_FechaInicio	=	Date(Today())

lstr_busq.argum[1] = String(gstr_param.plde_codigo)
lstr_busq.argum[2] = '1'							// Movimiento de Recepción
lstr_busq.argum[3] = ''								// Cualquier Estado
lstr_busq.argum[4] = String(ld_FechaInicio)  // Desde Fecha de Inicio Ducha
lstr_busq.argum[5] = "1"							// No muestra Productor

OpenWithParm(w_busc_spro_movtofrutagranenca, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	IF Date(ld_fechainicio) > Date(Mid(lstr_busq.argum[4],1,10))THEN
		Messagebox("Error", "La fecha de ducha debe ser menor que la fecha" + &
									" de la guia de Recepción") 
		RETURN
	ELSE
		ii_planta						=	Integer(lstr_busq.argum[1])
		ii_tipomovto					=	Integer(lstr_busq.argum[2])
		istr_mant.argumento[5]		=	lstr_busq.argum[8]
		dw_3.Object.guia_recepc[1]	=	Long(lstr_busq.Argum[3])
		
		pb_lectura.SetFocus()
	END IF
END IF

end subroutine

public subroutine chequeatipocontrol ();IF NOT IsNull(iuo_duchacontrol.ii_vari_codigo) THEN
	ii_tipocontrol	=	4
	RETURN
ELSEIF iuo_duchacontrol.ii_grva_codsub > 0 THEN
	ii_tipocontrol	=	3
	RETURN
ELSEIF NOT IsNull(iuo_duchacontrol.ii_grva_codigo) THEN
	ii_tipocontrol	=	2
	RETURN
ELSE
	ii_tipocontrol	=	1
	RETURN
END IF
end subroutine

public function boolean existeducha (string as_columna, string as_valor);Boolean	lb_Retorno
Integer	li_Ducha, li_Estanque, li_Cantidad
Date		ld_Fecha, ld_FechaNula
Time		lt_Hora

li_Ducha		= dw_3.Object.duch_codigo[1]
li_Estanque	= dw_3.Object.codu_nropos[1]
ld_Fecha		= dw_3.Object.codu_fecini[1]
lt_Hora		= dw_3.Object.codu_horini[1]

CHOOSE CASE as_Columna
	CASE "duch_codigo"
		li_Ducha		=	Integer(as_Valor)
		
	CASE "codu_nropos"
		li_Estanque	=	Integer(as_Valor)
		
	CASE "codu_fecini"
		ld_Fecha		=	Date(as_Valor)
		
	CASE "codu_horini"
		lt_Hora		=	Time(as_Valor)
		
END CHOOSE

IF NOT IsNull(li_Ducha) AND NOT IsNull(li_Estanque) AND NOT IsNull(ld_Fecha) AND &
	NOT IsNull(lt_Hora) THEN
	IF NOT iuo_duchacontrol.Existe(SqlCa,li_Ducha,li_Estanque,ld_Fecha,lt_Hora,True) THEN
		lb_Retorno = False
	ELSE
		ChequeaTipoControl()
		lb_Retorno = True
	END IF
END IF

lb_Retorno = False

RETURN lb_Retorno
end function

event ue_nuevo();//istr_mant.borra	= False
//istr_mant.agrega	= True
//
//OpenWithParm(iw_mantencion, istr_mant)
//
//IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
//	pb_eliminar.Enabled	= TRUE
//	pb_grabar.Enabled		= TRUE
//END IF
//
//dw_1.SetRow(il_fila)
//dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir();//
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long		ll_fila, ll_fila2, respuesta, ll_Numero
Integer	li_fila, li_Ducha, li_Pozo, li_Especie, li_Grupo, li_SubGrupo, li_Variedad
Date		ld_Fecha
Time	 	lt_Hora
String 	ls_fecha, ls_hora

ll_Numero						=	dw_3.Object.guia_recepc[1]
li_Ducha							=	dw_3.Object.duch_codigo[1]
li_Pozo							=	dw_3.Object.codu_nropos[1]
ld_Fecha							=	dw_3.Object.codu_fecini[1]
ls_fecha    					=  String(dw_3.Object.codu_fecini[1])
ls_hora     					=  String(dw_3.Object.codu_horini[1])
ls_hora                    =  ls_hora+':00'
lt_Hora							=	Time(ls_Hora)
dw_3.Object.codu_horini[1]	=  Time(ls_Hora)

ll_fila	= dw_1.Retrieve(li_Ducha,li_Pozo,ld_Fecha,lt_Hora,ii_Planta, ii_Tipomovto,&
                         ll_Numero)

IF ll_fila = -1 THEN
	MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.")
	pb_Nuevo.TriggerEvent(Clicked!)
	
	RETURN
ELSEIF ll_fila > 0 THEN
	MessageBox("Atención","Guía de Recepción ya fue pasada por ducha")
   pb_grabar.Enabled	   = True
	HabilitaEncab(False)
	
	
	RETURN
END IF

IF IsNull(iuo_duchacontrol.ii_espe_codigo) THEN
	li_Especie	=	0
ELSE	
	li_Especie	=	iuo_duchacontrol.ii_espe_codigo
END IF

IF IsNull(iuo_duchacontrol.ii_grva_codigo) THEN
	li_Grupo	=	0
ELSE	
	li_Grupo	=	iuo_duchacontrol.ii_grva_codigo
END IF

IF IsNull(iuo_duchacontrol.ii_grva_codsub) THEN
	li_SubGrupo	=	0
ELSE	
	li_SubGrupo	=	iuo_duchacontrol.ii_grva_codsub
END IF

IF IsNull(iuo_duchacontrol.ii_vari_codigo) THEN
	li_Variedad	=	0
ELSE	
	li_Variedad	=	iuo_duchacontrol.ii_vari_codigo
END IF

ll_fila2	=	dw_2.Retrieve(ii_planta, ii_tipomovto, ll_Numero, ii_TipoControl, &
								  li_Especie, li_Grupo, li_SubGrupo, li_Variedad)

IF ll_fila2 = -1 THEN
	MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.")
	pb_Nuevo.TriggerEvent(Clicked!)
	RETURN
ELSE
	IF ll_fila2 = 0 THEN
	   IF IsNull(iuo_duchacontrol.ii_espe_codalt) THEN
		   li_Especie	=	0
		ELSE	
			li_Especie	=	iuo_duchacontrol.ii_espe_codalt
		END IF
		
		IF IsNull(iuo_duchacontrol.ii_grva_codalt) THEN
			li_Grupo	=	0
		ELSE	
			li_Grupo	=	iuo_duchacontrol.ii_grva_codalt
		END IF
		
		IF IsNull(iuo_duchacontrol.ii_grva_subalt) THEN
			li_SubGrupo	=	0
		ELSE	
			li_SubGrupo	=	iuo_duchacontrol.ii_grva_subalt
		END IF
		
		IF IsNull(iuo_duchacontrol.ii_vari_codalt) THEN
			li_Variedad	=	0
		ELSE	
			li_Variedad	=	iuo_duchacontrol.ii_vari_codalt
		END IF
	
		ll_fila2	=	dw_2.Retrieve(ii_planta, ii_tipomovto, ll_Numero, ii_TipoControl, &
									  li_Especie, li_Grupo, li_SubGrupo, li_Variedad)

		IF ll_fila2 = -1 THEN
			MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.")
			pb_Nuevo.TriggerEvent(Clicked!)
			RETURN
		END IF
		IF ll_fila2 = 0 THEN
			MessageBox(	"Atencion", "No Existen Lotes Asociados.",Information!)
			pb_Nuevo.TriggerEvent(Clicked!)
			RETURN
      ELSE
			FOR li_fila = 1 TO dw_2.RowCount()
				 dw_1.InsertRow(li_fila)
				 dw_1.Object.duch_codigo[li_fila] 				=	dw_3.Object.duch_codigo[1]
				 dw_1.Object.codu_nropos[li_fila] 				=	dw_3.Object.codu_nropos[1]
				 dw_1.Object.codu_fecini[li_fila] 				=	dw_3.Object.codu_fecini[1]
				 dw_1.Object.codu_horini[li_fila] 				=	dw_3.Object.codu_horini[1]
				 dw_1.Object.lote_pltcod[li_fila] 				=	dw_2.Object.lote_pltcod[li_fila]
				 dw_1.Object.lote_espcod[li_fila] 				=	dw_2.Object.lote_espcod[li_fila]
				 dw_1.Object.lote_codigo[li_fila] 				=	dw_2.Object.lote_codigo[li_fila]
				 dw_1.Object.lote_totbul[li_fila] 				=	dw_2.Object.lote_totbul[li_fila]
				 dw_1.Object.lote_totnet[li_fila] 				= 	dw_2.Object.lote_totnet[li_fila]
				 dw_1.Object.prod_codigo[li_fila] 				= 	dw_2.Object.prod_codigo[li_fila]
				 dw_1.Object.productores_prod_nombre[li_fila]= 	dw_2.Object.productores_prod_nombre[li_fila]
				 dw_1.Object.vari_codigo[li_fila] 				= 	dw_2.Object.vari_codigo[li_fila]
				 dw_1.Object.variedades_vari_nombre[li_fila] = 	dw_2.Object.variedades_vari_nombre[li_fila]
			NEXT
		    	 pb_imprimir.Enabled		= True
		    	 pb_eliminar.Enabled		= True
		    	 pb_grabar.Enabled		= True
		END IF			
	ELSE
		FOR li_fila = 1 TO dw_2.RowCount()
			 dw_1.InsertRow(li_fila)
			 dw_1.Object.duch_codigo[li_fila] 				=	dw_3.Object.duch_codigo[1]
			 dw_1.Object.codu_nropos[li_fila] 				=	dw_3.Object.codu_nropos[1]
			 dw_1.Object.codu_fecini[li_fila] 				=	dw_3.Object.codu_fecini[1]
			 dw_1.Object.codu_horini[li_fila] 				=	dw_3.Object.codu_horini[1]
			 dw_1.Object.lote_pltcod[li_fila] 				=	dw_2.Object.lote_pltcod[li_fila]
			 dw_1.Object.lote_espcod[li_fila] 				=	dw_2.Object.lote_espcod[li_fila]
			 dw_1.Object.lote_codigo[li_fila] 				=	dw_2.Object.lote_codigo[li_fila]
			 dw_1.Object.lote_totbul[li_fila] 				=	dw_2.Object.lote_totbul[li_fila]
			 dw_1.Object.lote_totnet[li_fila] 				= 	dw_2.Object.lote_totnet[li_fila]
			 dw_1.Object.prod_codigo[li_fila] 				= 	dw_2.Object.prod_codigo[li_fila]
			 dw_1.Object.productores_prod_nombre[li_fila] = 	dw_2.Object.productores_prod_nombre[li_fila]
			 dw_1.Object.vari_codigo[li_fila] 				= 	dw_2.Object.vari_codigo[li_fila]
			 dw_1.Object.variedades_vari_nombre[li_fila] 	= 	dw_2.Object.variedades_vari_nombre[li_fila]
		NEXT
		    pb_imprimir.Enabled	= True
		    pb_eliminar.Visible	= True
			 pb_eliminar.Enabled	= True
		    pb_grabar.Enabled	= True
	END IF
END IF

end event

on w_mant_mues_spro_lotesduchados.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_2
this.Control[iCurrent+3]=this.dw_3
end on

on w_mant_mues_spro_lotesduchados.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.dw_3)
end on

event ue_borrar();IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

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
	dw_1.SelectRow(il_fila,True)
END IF

istr_mant.borra	 = False
end event

event open;call super::open;buscar	= "Lote:Nlote_codigo,Productor:Nprod_codigo,Variedad:Nvari_codigo"
ordenar	= "Lote:lote_codigo,Productor:prod_codigo,Variedad:vari_codigo"

iuo_spro_movtofrutagranenca		=	Create uo_spro_movtofrutagranenca
iuo_duchacontrol						=	Create uo_duchacontrol

dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)

dw_3.InsertRow(0)
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_antesguardar();call super::ue_antesguardar;Integer	li_Secuencia, li_Ducha, li_Pozo
Long		ll_Fila
Date		ld_Fecha
Time		lt_Hora

li_Ducha		=	dw_3.Object.duch_codigo[1]
li_Pozo		=	dw_3.Object.codu_nropos[1]
ld_Fecha		=	dw_3.Object.codu_fecini[1]
lt_Hora		=	dw_3.Object.codu_horini[1]

SELECT	IsNull(Max(lodu_secuen), 0) + 1
	INTO	:li_Secuencia
	FROM	dba.spro_lotesduchados
	WHERE	duch_codigo	=	:li_Ducha
	AND	codu_nropos	=	:li_Pozo
	AND	codu_fecini	=	:ld_Fecha
	AND	codu_horini	=	:lt_Hora;

FOR ll_Fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(1, 0, Primary!) = NewModified! THEN
		dw_1.Object.lodu_secuen[ll_Fila]	=	li_Secuencia
		li_Secuencia ++
	END IF
NEXT
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_spro_lotesduchados
integer x = 82
integer y = 632
integer width = 2766
integer height = 1268
integer taborder = 60
string dataobject = "dw_mant_mues_spro_lotesduchados"
boolean hscrollbar = true
end type

event dw_1::doubleclicked;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_spro_lotesduchados
boolean visible = false
integer y = 64
integer width = 2107
integer height = 408
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_spro_lotesduchados
integer x = 2990
integer y = 148
integer taborder = 70
end type

event pb_lectura::clicked;IF dw_3.Object.duch_codigo[1] = 0 OR Isnull(dw_3.Object.duch_codigo[1]) THEN
	MessageBox("Atención","Faltan ingresar Ducha Control")
	RETURN
ELSEIF dw_3.Object.guia_recepc[1] = 0 OR Isnull(dw_3.Object.guia_recepc[1]) THEN
	MessageBox("Atención","Faltan ingresar Guia de Recepción")
	RETURN
END IF

Parent.TriggerEvent("ue_recuperadatos")
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_spro_lotesduchados
integer x = 2976
integer y = 452
integer taborder = 80
end type

event pb_nuevo::clicked;call super::clicked;String	ls_Null

SetNull(ls_Null)

HabilitaEncab(TRUE)

dw_3.Object.guia_recepc[1] = Long(ls_Null)
dw_3.SetFocus()
dw_3.SetColumn("guia_recepc")
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_spro_lotesduchados
boolean visible = false
integer x = 2469
integer y = 640
integer taborder = 90
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_spro_lotesduchados
boolean visible = false
integer x = 2469
integer y = 868
integer taborder = 100
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_spro_lotesduchados
integer x = 2976
integer y = 1084
integer taborder = 110
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_spro_lotesduchados
boolean visible = false
integer x = 2469
integer y = 1300
integer taborder = 120
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_spro_lotesduchados
integer x = 2976
integer y = 1516
integer height = 136
integer taborder = 130
end type

type st_1 from statictext within w_mant_mues_spro_lotesduchados
integer x = 73
integer y = 64
integer width = 2117
integer height = 556
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_mant_mues_spro_lotesduchados
boolean visible = false
integer x = 229
integer y = 1284
integer width = 2245
integer height = 556
integer taborder = 140
boolean bringtotop = true
string dataobject = "dw_gene_spro_lotesduchados"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_mant_mues_spro_lotesduchados
integer x = 69
integer y = 60
integer width = 2107
integer height = 556
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_sele_duchacontrol"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event buttonclicked;Str_Busqueda	lstr_busq

CHOOSE CASE dwo.Name
	CASE "buscaducha"
		
		buscaducha()
		
	CASE "buscarecepcion"
		
		buscarecepcion()
END CHOOSE
end event

event itemchanged;String	ls_Columna, ls_Null
Date 		ld_fechaducha
SetNull(ls_Null)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "duch_codigo", "codu_nropos", "codu_fecini", "codu_horini"
	   ExisteDucha(ls_Columna, Data)
		ld_fechaducha = Date(Data)
			
	CASE "guia_recepc"

		ld_fechaducha = Date(dw_3.Object.codu_fecini[row])
		IF NOT iuo_spro_movtofrutagranenca.Existe(gstr_param.plde_codigo,1,Long(data), True, Sqlca) THEN
			This.Object.guia_recepc[row]	=	Long(ls_null)
			RETURN 1
		ELSEIF Not (ld_fechaducha > Date(iuo_spro_movtofrutagranenca.idt_mfge_fecmov)) THEN
			
				 ii_planta					=	iuo_spro_movtofrutagranenca.ii_plde_codigo
				 ii_tipomovto				=	iuo_spro_movtofrutagranenca.ii_tpmv_codigo
			 ELSE
		 		Messagebox("Error", "La Fecha de la  Guia de Recepción es Menor" + &
										  "que la Fecha de la Ducha ")
				This.Object.guia_recepc[row]	=	Long(ls_null)										  
				RETURN
		END IF

END CHOOSE
end event

event itemerror;RETURN 1
end event

