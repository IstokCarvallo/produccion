$PBExportHeader$w_mant_mues_anula_inspeccion.srw
$PBExportComments$MantenciónAnulación de Inspección por Vencimiento.
forward
global type w_mant_mues_anula_inspeccion from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_anula_inspeccion
end type
type em_fecha from editmask within w_mant_mues_anula_inspeccion
end type
type st_3 from statictext within w_mant_mues_anula_inspeccion
end type
type st_2 from statictext within w_mant_mues_anula_inspeccion
end type
type em_anula from editmask within w_mant_mues_anula_inspeccion
end type
type st_planta from statictext within w_mant_mues_anula_inspeccion
end type
type st_5 from statictext within w_mant_mues_anula_inspeccion
end type
type st_6 from statictext within w_mant_mues_anula_inspeccion
end type
type cbx_seedle from checkbox within w_mant_mues_anula_inspeccion
end type
type st_estado from statictext within w_mant_mues_anula_inspeccion
end type
type st_4 from statictext within w_mant_mues_anula_inspeccion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_anula_inspeccion
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_anula_inspeccion
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_anula_inspeccion
end type
type uo_selvariedad from uo_seleccion_variedad within w_mant_mues_anula_inspeccion
end type
end forward

global type w_mant_mues_anula_inspeccion from w_mant_tabla
integer width = 3291
integer height = 2176
string title = "ANULACION DE INSPECCION POR VENCIMIENTO"
st_1 st_1
em_fecha em_fecha
st_3 st_3
st_2 st_2
em_anula em_anula
st_planta st_planta
st_5 st_5
st_6 st_6
cbx_seedle cbx_seedle
st_estado st_estado
st_4 st_4
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
end type
global w_mant_mues_anula_inspeccion w_mant_mues_anula_inspeccion

type variables

end variables

forward prototypes
public function boolean existeespecie (integer especie)
public function boolean noexistepallet (string as_pallet)
public function boolean duplicado (string as_pallet)
end prototypes

public function boolean existeespecie (integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dbo.especies
	WHERE	espe_codigo	=	:Especie ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Especies")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

public function boolean noexistepallet (string as_pallet);//String	ls_variedad, ls_embalaje, ls_especie
//Integer	li_cliente, li_tipopa, li_inspec, li_tipoin, li_planta, li_secuen, li_estado, li_fumiga
Integer	li_Cliente,li_Planta,li_Inspec,li_Estado,li_Secuen,li_Tipopa,li_Especie,&
			li_Variedad,li_VariFiltro,li_Seedles,li_Destino
Long		ll_Numero, ll_Cajas,ll_Num_Inpec
String	ls_Especie,ls_Variedad,ls_Embalaje
Date		ld_Fecha

li_Cliente		= uo_SelCliente.Codigo
li_Planta		= uo_SelPlanta.Codigo
ll_numero 		= Long(as_pallet)

li_VariFiltro 	= Integer(istr_mant.argumento[9])
li_Seedles		= Integer(istr_mant.argumento[10]) 	// '1'

			SELECT	pae.paen_estado,pae.paen_inspec,pae.paen_tipopa,pae.espe_codigo,pae.vari_codigo,
						esp.espe_nombre,var.vari_nombre,pae.emba_codigo,pae.paen_ccajas,pae.dest_codigo
				INTO	:li_Estado,:li_Inspec,:li_Tipopa,:li_Especie,:li_Variedad,
						:ls_Especie,:ls_Variedad,:ls_Embalaje,:ll_Cajas,:li_Destino
				FROM	dbo.palletencab as pae, dbo.especies as esp,
						dbo.variedades as var
				WHERE	pae.clie_codigo = :li_Cliente	AND
						pae.plde_codigo = :li_Planta	AND
						pae.paen_numero = :ll_Numero	AND
						:li_VariFiltro in (-1,pae.vari_codigo) AND			
						pae.espe_codigo = esp.espe_codigo AND
						pae.espe_codigo = var.espe_codigo AND
						pae.vari_codigo = var.vari_codigo AND
						Isnull(var.vari_seedle,0) = :li_Seedles;
			
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca, "Lectura Información de Pallet")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	//MessageBox("Atención", "Número de Pallet no ha sido creado.~rIngrese otro Número.", + &
	MessageBox("Atención", "Número de Pallet no Correspnde a las Condiciones.~rIngrese otro Número.", + &
	Exclamation!, OK!)
	RETURN True

ELSEIF li_Estado = 2 THEN
	MessageBox("Atención","Pallet fue Despachado.~r~r" + &
				"Ingrese o seleccione otro Pallet.")
	RETURN True
ELSEIF li_Estado = 3 THEN
	MessageBox("Atención","Pallet fue Repalletizado.~r~r" + &
				"Ingrese o seleccione otro Pallet.")
	RETURN True
ELSEIF IsNull(li_Inspec) OR li_Inspec = 0 THEN
	MessageBox("Atención","Nro. de Pallet no ha sido previamente Inspeccionado.~r~r" + &
					"Ingrese o seleccione otro Pallet.")
	RETURN True		
ELSEIF (li_inspec = 1 OR li_inspec = 2) THEN	// Pallet en Existencia Inspeccionado o Reinspeccionado
	SELECT	max(inpe_secuen),max(inpe_numero),max(inpd_fechai)
		INTO	:li_Secuen,:ll_Num_Inpec,:ld_Fecha
		FROM	dbo.inspecpaldet
		WHERE	clie_codigo = :li_Cliente
		AND	plde_codigo = :li_Planta
		AND	paen_numero	= :ll_Numero
		AND   isnull(inpd_nroanu,0) = 0;

	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura Información de Inspección de Pallet")
		RETURN True
	ELSEIF sqlca.SQLCode = 100 THEN
		//MessageBox("Atención","Pallet ya fue considerado en otra Inspección.~r~r" + &
		//			"Ingrese o seleccione otro Pallet.")
		//RETURN True
	ELSEIF li_Secuen > 0 THEN
			dw_1.Object.clie_codigo[il_Fila]	=	 uo_SelCliente.Codigo
			dw_1.Object.plde_codigo[il_Fila]	=	 uo_SelPlanta.Codigo
			dw_1.Object.paen_numero[il_Fila] = ll_Numero
			dw_1.Object.paen_tipopa[il_Fila] = li_Tipopa
			dw_1.Object.espe_codigo[il_Fila] = li_Especie
			dw_1.Object.espe_nombre[il_Fila] = ls_Especie
			dw_1.Object.vari_codigo[il_Fila]	= li_Variedad
			dw_1.Object.vari_nombre[il_Fila] = ls_Variedad
			dw_1.Object.emba_codigo[il_Fila] = ls_Embalaje
			dw_1.Object.paen_ccajas[il_Fila] = ll_Cajas
			dw_1.Object.inpe_tipoin[il_Fila] = li_Inspec
			dw_1.Object.inpe_numero[il_Fila] = ll_Num_Inpec
			dw_1.Object.inpd_fechai[il_Fila] = ld_Fecha				
			dw_1.Object.dest_codigo[il_Fila] = li_Destino
			dw_1.Object.inpe_secuen[il_Fila] = li_Secuen
			RETURN False
	ELSE
		MessageBox("Atención","Nro. de Pallet no ha sido previamente Inspeccionado.~r~r" + &
				"Ingrese o seleccione otro Pallet.")
		RETURN True			
	END IF		
END IF


end function

public function boolean duplicado (string as_pallet);Long		ll_fila

ll_fila	= dw_1.Find("paen_numero = " + as_pallet, 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Número de Pallet ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

il_fila = dw_1.InsertRow(0)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= 	TRUE
	pb_grabar.Enabled		= 	TRUE
END IF
ib_Borrar	=	False

dw_1.ScrollToRow(il_fila)
dw_1.SetRow(il_Fila)
dw_1.SetFocus()
dw_1.SetColumn(1)
end event

event ue_imprimir;//
end event

event ue_recuperadatos;Long		ll_Fila, ll_Respuesta
Integer	li_SeedLess

li_SeedLess  =  Integer(istr_mant.argumento[10])

DO
	ll_Fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date(em_Fecha.Text), Long(em_anula.Text),&
	                         uo_SelEspecie.Codigo, uo_SelVariedad.Codigo, li_SeedLess)
				  
	IF ll_Fila = -1 THEN
		ll_Respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_Fila > 0 AND em_anula.text <> ""THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_grabar.Enabled		= True
		pb_insertar.Enabled	=	True
			
		IF IsNull(dw_1.Object.anula[1]) OR dw_1.Object.anula[1] = 0 THEN
			st_estado.text = 'Pendiente'
		ELSE
			st_estado.text = 'Anulado'
			pb_grabar.Enabled		= FALSE
		END IF
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE ll_Respuesta = 1

IF ll_Respuesta = 2 THEN Close(This)
end event

on w_mant_mues_anula_inspeccion.create
int iCurrent
call super::create
this.st_1=create st_1
this.em_fecha=create em_fecha
this.st_3=create st_3
this.st_2=create st_2
this.em_anula=create em_anula
this.st_planta=create st_planta
this.st_5=create st_5
this.st_6=create st_6
this.cbx_seedle=create cbx_seedle
this.st_estado=create st_estado
this.st_4=create st_4
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.em_fecha
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_anula
this.Control[iCurrent+6]=this.st_planta
this.Control[iCurrent+7]=this.st_5
this.Control[iCurrent+8]=this.st_6
this.Control[iCurrent+9]=this.cbx_seedle
this.Control[iCurrent+10]=this.st_estado
this.Control[iCurrent+11]=this.st_4
this.Control[iCurrent+12]=this.uo_selcliente
this.Control[iCurrent+13]=this.uo_selplanta
this.Control[iCurrent+14]=this.uo_selespecie
this.Control[iCurrent+15]=this.uo_selvariedad
end on

on w_mant_mues_anula_inspeccion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.em_fecha)
destroy(this.st_3)
destroy(this.st_2)
destroy(this.em_anula)
destroy(this.st_planta)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.cbx_seedle)
destroy(this.st_estado)
destroy(this.st_4)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0
This.TriggerEvent ("ue_validaborrar")
IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = True
		pb_eliminar.Enabled = True
		pb_grabar.Enabled = True
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN

	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelVariedad.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelVariedad.Seleccion(True, False)
	
	uo_SelPlanta.Filtra(1)
	uo_SelVariedad.Filtra(gi_CodEspecie)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	uo_SelEspecie.Inicia(gi_CodEspecie)

	em_fecha.Text				=	String(Today())

	istr_mant.argumento[9]	= 	"-1" 				// variedad
	istr_mant.argumento[10] =  "0"            		// Seedle 
	istr_mant.argumento[3]  =  em_fecha.Text

	buscar	=	"Número de Pallet:Npaen_numero,Estado Anterior:Ncoca_estant,Estado Nuevo:Ncoca_estneo"
	ordenar	=	"Número de Pallet:paen_numero,Estado Anterior:coca_estant,Estado Nuevo:coca_estneo"
End If
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

//	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_Fila, ll_Numero, ll_Conteo, ll_Pallet
Date		ld_FechaAnula
String		ls_Nulo, ls_certificado

SetNull(ls_Nulo)

ll_Numero		=	Long(em_anula.Text)
ld_FechaAnula	=	Date(em_fecha.Text)
ll_Fila			=	dw_1.GetSelectedRow(0)

dw_1.ResetUpdate()

FOR ll_Conteo = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_Conteo, 0, Primary!) = New! OR IsNull(dw_1.Object.paen_numero[ll_Conteo]) THEN
		dw_1.DeleteRow(ll_Conteo)
	ELSE	
		IF IsNull(dw_1.Object.inpd_nrocer[ll_conteo]) OR dw_1.Object.inpd_nrocer[ll_conteo] = '' THEN
			MessageBox("Atención","Falta N° Certificado.")
			Message.DoubleParm = -1
			Return
		ELSE
			ls_certificado = dw_1.Object.inpd_nrocer[ll_conteo]
		END IF	
		dw_1.SetItem(ll_Conteo, "inpd_nroanu", ll_Numero)
		dw_1.SetItem(ll_Conteo, "inpd_fechaa", ld_FechaAnula)
		dw_1.SetItem(ll_Conteo, "inpd_nrocer", ls_certificado)
	END IF
NEXT
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")
Message.DoubleParm = 0
TriggerEvent("ue_antesguardar")
IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

IF wf_actualiza_db() = TRUE THEN
	st_estado.text = 'Anulado'
ELSE
	st_estado.text = 'Pendiente'
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_anula_inspeccion
integer x = 55
integer y = 700
integer width = 2670
integer height = 1232
integer taborder = 120
string dataobject = "dw_mues_anula_inspecpaldet_pro"
boolean hscrollbar = true
end type

event dw_1::clicked;call super::clicked;String		ls_Tecla

IF KeyDown(KeyShift!) THEN
	ls_tecla	=	"Shift"
ELSEIF KeyDown(KeyControl!) THEN
	ls_tecla	=	"Control"
END IF

F_Selecciona(This, ls_Tecla, Row)

IF dw_1.GetSelectedRow(0) = 0 THEN
	pb_grabar.Enabled	=	False
ELSE
	pb_grabar.Enabled	=	True
END IF




end event

event dw_1::getfocus;//
end event

event dw_1::losefocus;//
end event

event dw_1::rowfocuschanged;//
end event

event dw_1::ue_seteafila;//
end event

event dw_1::itemchanged;call super::itemchanged;String	ls_columna
Long		ll_null

SetNull(ll_null)

dw_1.SetFocus()
dw_1.SetColumn(1)

ls_columna	=	dwo.Name
 CHOOSE CASE ls_columna
 	CASE "paen_numero"
		IF Duplicado(data) THEN
			This.SetItem(row,ls_columna,ll_null)	
			pb_grabar.Enabled = False
			pb_eliminar.Enabled = False
			RETURN 1	
		ELSEIF NoExistePallet(data) THEN			
			This.SetItem(row,ls_columna,ll_null)			
   		RETURN 1
		END IF
 END CHOOSE
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_anula_inspeccion
integer x = 50
integer y = 60
integer width = 2679
integer height = 624
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_anula_inspeccion
integer x = 2875
integer y = 92
integer taborder = 100
boolean enabled = false
end type

event pb_lectura::clicked;uo_SelCliente.Bloquear(True)
uo_SelPlanta.Bloquear(True)
uo_SelEspecie.Bloquear(True)

em_anula.Enabled		=	False
em_fecha.Enabled		=	False

Parent.TriggerEvent("ue_recuperadatos")

pb_insertar.Enabled	= True

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= 	TRUE
	//pb_grabar.Enabled		= 	TRUE
END IF




end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_anula_inspeccion
integer x = 2875
integer y = 412
integer taborder = 110
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)
uo_SelPlanta.Bloquear(False)
uo_SelEspecie.Bloquear(False)

em_anula.Enabled		=	True
em_anula.text        = ''
em_fecha.Enabled		=	True
em_fecha.Text			=	String(Today())
st_estado.text       = ''
cbx_seedle.Checked   =  False
pb_lectura.Enabled	=	False

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_anula_inspeccion
integer x = 2875
integer y = 676
integer taborder = 130
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_anula_inspeccion
boolean visible = false
integer x = 2875
integer y = 796
integer taborder = 140
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_anula_inspeccion
integer x = 2875
integer y = 984
integer taborder = 150
end type

event pb_grabar::clicked;Integer li_Fila,li_Pallet

Parent.TriggerEvent("ue_guardar")

li_Fila=dw_1.RowCount()

IF li_Fila > 0 THEN
	li_Pallet = dw_1.GetItemNumber(li_Fila,"paen_numero")
ELSE
	pb_Grabar.Enabled = False
END IF
end event

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_anula_inspeccion
boolean visible = false
integer x = 2875
integer y = 1176
integer taborder = 160
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_anula_inspeccion
integer x = 2875
integer y = 1520
integer taborder = 170
end type

type st_1 from statictext within w_mant_mues_anula_inspeccion
integer x = 146
integer y = 104
integer width = 274
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_anula_inspeccion
integer x = 2213
integer y = 224
integer width = 425
integer height = 92
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
end type

event modified;IF Not f_validafechatempo(date(this.Text)) THEN
	This.Text = ''
	This.SetFocus()
END IF
istr_mant.argumento[3]  =  em_fecha.Text
		
end event

type st_3 from statictext within w_mant_mues_anula_inspeccion
integer x = 1966
integer y = 232
integer width = 206
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha"
alignment alignment = right!
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_anula_inspeccion
integer x = 1618
integer y = 116
integer width = 553
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Anulación S.A.G."
alignment alignment = right!
boolean focusrectangle = false
end type

type em_anula from editmask within w_mant_mues_anula_inspeccion
integer x = 2213
integer y = 92
integer width = 425
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
string displaydata = "@"
end type

event modified;Long		ll_Fila, ll_Respuesta, ll_Numero
Date		ld_FechaAnula, ld_Fecha

ll_Numero			=	Long(em_anula.Text)

//ii_existe			= 0
IF This.Text <> "" THEN
	SELECT DISTINCT inpd_fechaa
		INTO	:ld_FechaAnula 
		FROM	dbo.inspecpaldet
		WHERE	clie_Codigo	=	:uo_SelCliente.Codigo
		AND	plde_codigo	=	:uo_SelPlanta.Codigo
		AND	inpd_nroanu	=	:ll_Numero ;
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de Detalle Inspección")
	ELSEIF Not IsNull(ld_FechaAnula) AND ld_FechaAnula <> ld_Fecha THEN
		em_fecha.Text			=	String(ld_FechaAnula)
		em_fecha.DisplayOnly	=	True
		//ii_existe				= 1
		pb_lectura.Enabled = True
		pb_lectura.SetFocus()
	ELSE
		pb_lectura.Enabled = True
		em_fecha.DisplayOnly	=	False
		em_fecha.SetFocus()
	END IF
END IF
end event

type st_planta from statictext within w_mant_mues_anula_inspeccion
integer x = 146
integer y = 228
integer width = 293
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_5 from statictext within w_mant_mues_anula_inspeccion
integer x = 146
integer y = 336
integer width = 274
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_mues_anula_inspeccion
integer x = 146
integer y = 552
integer width = 302
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Variedad"
boolean focusrectangle = false
end type

type cbx_seedle from checkbox within w_mant_mues_anula_inspeccion
integer x = 2213
integer y = 536
integer width = 402
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Seedless"
end type

event clicked;IF This.Checked THEN
	istr_mant.argumento[10] = '1'
ELSE
	istr_mant.argumento[10] = '0'
END IF
end event

type st_estado from statictext within w_mant_mues_anula_inspeccion
integer x = 2213
integer y = 352
integer width = 425
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 128
long backcolor = 553648127
boolean enabled = false
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_anula_inspeccion
integer x = 1966
integer y = 364
integer width = 206
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Estado"
alignment alignment = right!
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_anula_inspeccion
event destroy ( )
integer x = 526
integer y = 100
integer height = 92
integer taborder = 90
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_anula_inspeccion
event destroy ( )
integer x = 526
integer y = 220
integer height = 92
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_mant_mues_anula_inspeccion
event destroy ( )
integer x = 526
integer y = 332
integer height = 92
integer taborder = 90
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_mant_mues_anula_inspeccion
integer x = 526
integer y = 460
integer taborder = 120
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

IF IsNull(This.SeedLess) OR This.SeedLess = 0 THEN
	cbx_seedle.Checked = FALSE
	cbx_seedle.Enabled = FALSE
	istr_mant.argumento[10] = '0'
ELSE
	cbx_seedle.Checked = TRUE
	cbx_seedle.Enabled = FALSE
	istr_mant.argumento[10] = '1'
END IF
end event

