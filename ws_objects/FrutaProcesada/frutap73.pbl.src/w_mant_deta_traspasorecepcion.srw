$PBExportHeader$w_mant_deta_traspasorecepcion.srw
forward
global type w_mant_deta_traspasorecepcion from w_mant_detalle_csd
end type
type dw_2 from uo_dw within w_mant_deta_traspasorecepcion
end type
type dw_3 from uo_dw within w_mant_deta_traspasorecepcion
end type
end forward

global type w_mant_deta_traspasorecepcion from w_mant_detalle_csd
integer width = 3570
integer height = 2232
string title = "Detalle Pallet"
dw_2 dw_2
dw_3 dw_3
end type
global w_mant_deta_traspasorecepcion w_mant_deta_traspasorecepcion

type variables
DataWindowChild   dw_calibres		,	dw_productor 	,	&
                  dw_embalajes	,	dw_variedades 	,	&
                  dw_plantadesp	,	dw_especies		,	&
                  dw_etiquetas	,	dw_condiciones	,	dw_cliente,&
						idwc_predio		, 	idwc_cuartel
						
Integer il_lote						


end variables

forward prototypes
public function boolean existevariecab (integer as_valor)
public function boolean noexistecalibre (string as_valor)
public function boolean duplicado (string campo)
public function boolean noexisteproductor (string ls_columna)
public subroutine buscaprod ()
public function boolean varificaproductor (long ll_productor)
public function boolean noexistepredio (long ai_productor, integer ai_predio)
public function boolean noexistefechaingreso (string ls_columna)
public function boolean noexistefechaembalaje (string ls_columna)
end prototypes

public function boolean existevariecab (integer as_valor);Integer	li_especie, li_variedad, li_cliente
String	ls_nombre, ls_calibre
			//ls_secacod

li_cliente					= integer(istr_mant.argumento[1])
li_especie					= integer(istr_mant.argumento[3])
li_variedad					= as_valor

//ls_secacod					= istr_mant.argumento[12]

SELECT	vaca_calibr    INTO : ls_calibre
	FROM	dba.variecalibre
	WHERE	espe_codigo		= :li_especie  and &
			vari_codigo    = :li_variedad ;//and &
	//		seca_codigo		= : ls_secacod ;
			
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variecalibre")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Variedad-Calibre no Existe.", Exclamation!, Ok!)
	RETURN False
END IF

RETURN TRUE
end function

public function boolean noexistecalibre (string as_valor);String	ls_codigo, ls_calibr
Integer	li_cliente, li_especie, li_variedad
Long	   registros

dw_1.accepttext()

li_cliente	= Integer(istr_mant.argumento[1])
li_especie	= Integer(istr_mant.argumento[3])
li_variedad	= Integer(istr_mant.argumento[4])
ls_codigo   = Upper(as_valor)


SELECT	vaca_calibr
	INTO	:ls_calibr
	FROM  dba.variecalibre
	WHERE	espe_codigo =	:li_especie
	AND	vari_codigo =	:li_variedad 
	AND	vaca_calibr	=	:ls_codigo ;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla VarieCalibre")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Calidad no Asignada para esta Variedad, Ingrese otra.", &
					Exclamation!, OK!)
	RETURN True
ELSE
	dw_1.SetItem(il_fila, "pafr_calibr", ls_codigo)
	RETURN False
END IF

end function

public function boolean duplicado (string campo);Long		ll_fila
String	ls_client, ls_nropal, ls_especi, ls_varied, ls_embala, ls_produc, ls_condic, ls_etique, &
			ls_planta, ls_zonaco, ls_calibr, ls_huert1, ls_cuart1

	
ls_client = istr_mant.argumento[1]
ls_nropal = istr_mant.argumento[2]
ls_especi = istr_mant.argumento[3]
ls_varied = istr_mant.argumento[4]
ls_embala = istr_mant.argumento[7]
ls_produc = String(dw_1.GetItemNumber(il_fila, "prod_codigo"))
ls_condic = istr_mant.argumento[10]
ls_etique = istr_mant.argumento[9]
ls_planta = istr_mant.argumento[6]
ls_calibr = dw_1.GetItemString(il_fila, "pafr_calibr")
ls_huert1 = String(dw_1.GetItemNumber(il_fila, "pafr_huert1"))
ls_cuart1 = String(dw_1.GetItemNumber(il_fila, "pafr_cuart1"))

ll_fila	= dw_1.Find( "clie_codigo = " + ls_client + &
						    " AND paen_numero = " + ls_nropal + &
						    " AND espe_codigo = " + ls_especi + &
							 " AND vari_codigo = " + ls_varied + &
							 " AND emba_codigo = '" + ls_embala + "'" + &
							 " AND prod_codigo = " + ls_produc + &
							 " AND cond_codigo = " + ls_condic + &
							 " AND etiq_codigo = " + ls_etique + &
							 " AND plde_codigo = " + ls_planta + &
							 " AND pafr_huert1 = " + ls_huert1 + &
							 " AND pafr_cuart1 = " + ls_cuart1 + &
							 " AND pafr_calibr = '" + ls_calibr + "'", 1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Calidad ya fue ingresada anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean noexisteproductor (string ls_columna);String	ls_nombre
Integer	li_cliente
Boolean	lb_retorna = True
Long		ll_codigo

li_cliente	 = Integer(istr_mant.argumento[1]) 
ll_codigo 	 = Long(ls_columna)

//IF istr_mant.argumento[17] = '1'and istr_mant.argumento[24] <> '1' THEN
//	IF Pos(istr_mant.argumento[15], "," + String(ll_codigo)) = 0 THEN
//		messagebox("Atención","Productor no corresponde a Pallets originales", Exclamation!, Ok!)
//		RETURN lb_Retorna
//	END IF
//END IF

IF istr_mant.argumento[17] = '1'and istr_mant.argumento[24] <> '1' THEN
	IF Pos(istr_mant.argumento[47], "," + String(ll_codigo)) = 0 THEN
		messagebox("Atención","Productor no corresponde a Pallets originales", Exclamation!, Ok!)
		RETURN lb_Retorna
	END IF
END IF

	SELECT	prod_nombre INTO :ls_nombre
	FROM    dba.PRODUCTORES
	WHERE	   prod_codigo = :ll_codigo ;
	
	IF sqlca.SQLCode = -1 THEN
		F_errorbasedatos(sqlca,"Lectura tabla Productores")
		Return lb_Retorna
		
	ELSEIF sqlca.SQLCode = 100 THEN
		MessageBox("Atención", "Código de Productor no ha sido creado, Ingrese otro Código.", &
		Exclamation!, OK!)
		Return lb_Retorna
	ELSE
		istr_mant.argumento[13]	=	string(ll_codigo)
		dw_1.SetItem (il_fila, "prod_codigo", ll_codigo)
		dw_1.SetItem (il_fila, "productores_prod_nombre", ls_nombre)
		lb_retorna = False
		RETURN lb_retorna
	END IF

Return lb_retorna
end function

public subroutine buscaprod ();dw_1.Modify("buscaprod.border = 0")
dw_1.Modify("buscaprod.border = 5")
istr_busq.Argum[1] = istr_mant.Argumento[1]

OpenWithParm(w_busc_productores, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.Argum[4] = "" THEN
	dw_1.SetColumn("prod_codigo")
	dw_1.SetFocus()
ELSE
	dw_1.SetItem(il_fila, "prod_codigo", Long(istr_busq.Argum[4]))
	dw_1.SetItem(il_fila, "productores_prod_nombre", istr_busq.Argum[5])
	IF istr_mant.argumento[14] = '1' THEN
		NoExisteProductor(istr_busq.Argum[4])
	END IF
	
	dw_1.SetColumn("pafr_calibr")
	dw_1.SetFocus()
END IF

dw_1.Modify("buscaprod.border = 0")
dw_1.Modify("buscaprod.border = 6")

RETURN
end subroutine

public function boolean varificaproductor (long ll_productor); Integer li_cont=0, li_cliente,li_planta
 
 li_cliente=Integer(istr_mant.argumento[1])
 li_planta=Integer(istr_mant.argumento[21])

 SELECT count (prod_codigo) 
 INTO :li_cont  
 FROM dba.prodpacking  
 WHERE prod_codigo = :ll_productor AND  
       plde_codigo = :li_planta;
			
IF (sqlca.sqlcode)= -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Prodpacking")
	RETURN FALSE
ELSEIF li_cont>0 THEN
	RETURN TRUE
ELSE
	MessageBox("Atención", "Código de Productor No Asignado a Packing Origen.", &
					Exclamation!, OK!)
	RETURN FALSE
END IF


end function

public function boolean noexistepredio (long ai_productor, integer ai_predio);Integer li_existe

SELECT Count(prod_codigo) 
INTO :li_existe
FROM dba.spro_prodpredio
WHERE prod_codigo = :ai_productor
AND	prpr_codigo = :ai_predio;

IF (sqlca.sqlcode)= -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_prediosproductor")
	RETURN FALSE
ELSEIF li_existe>0 THEN
	RETURN TRUE
ELSE
	MessageBox("Atención", "Codigo de Predio no esta asignado a Productor.", &
					Exclamation!, OK!)
	RETURN FALSE
END IF
end function

public function boolean noexistefechaingreso (string ls_columna);String	ls_nombre
Date ld_pafr_fecing
Boolean	lb_retorna =False

ld_pafr_fecing = Date(ls_columna)

IF istr_mant.argumento[45]<> ''  AND  istr_mant.argumento[45]<> ',' THEN
	IF Pos(istr_mant.argumento[45], "," + String(ld_pafr_fecing)) = 0 THEN
		messagebox("Atención","Fecha de Ingreso no corresponde a Pallets originales", Exclamation!, Ok!)
		 lb_Retorna	= True
	END IF
END IF
 
RETURN lb_Retorna 

end function

public function boolean noexistefechaembalaje (string ls_columna);String	ls_nombre
Date ld_pafr_fecemb
Boolean	lb_retorna =False

ld_pafr_fecemb = Date(ls_columna)

IF istr_mant.argumento[46]<> '' AND  istr_mant.argumento[46]<> ',' THEN
	IF Pos(istr_mant.argumento[46], "," + String(ld_pafr_fecemb)) = 0 THEN
		messagebox("Atención","Fecha de Embalaje no corresponde a Pallets originales", Exclamation!, Ok!)
		 lb_Retorna	= True
	END IF
END IF

RETURN lb_Retorna  

end function

on w_mant_deta_traspasorecepcion.create
int iCurrent
call super::create
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_2
this.Control[iCurrent+2]=this.dw_3
end on

on w_mant_deta_traspasorecepcion.destroy
call super::destroy
destroy(this.dw_2)
destroy(this.dw_3)
end on

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_Fila, Respuesta

DO
	dw_2.SetRedraw(False)
	dw_2.Reset()
	ll_Fila	=	dw_2.Retrieve(Long(istr_Mant.Argumento[1]), Long(istr_Mant.Argumento[2]))
	
	IF ll_Fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_Fila = 0 Then 
		MessageBox("Error en Base de Datos", "No existen recepciones.", Information!, RetryCancel!)
	END IF
	dw_2.SetRedraw(True)
LOOP WHILE respuesta = 1

IF Respuesta = 2 THEN Close(This)

end event

event open;x	= 100
y	= 450

PostEvent("ue_recuperadatos")

istr_mant = Message.PowerObjectParm

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)

istr_mant.dw.ShareData(dw_1)

end event

event resize;call super::resize;dw_2.x = dw_1.x
dw_2.y = dw_1.y
dw_2.Height = dw_1.Height
dw_2.Width = dw_1.Width

dw_3.y = dw_2.y + dw_2.Height + 50
dw_3.x = dw_2.x

This.Height			=	dw_1.Height + dw_3.Height  + 300
end event

event ue_nuevo;Long	ll_New, ll_Busca, ll_Fila
String	ls_Busca

For ll_Fila = 1 To dw_3.RowCount()
	If dw_3.IsSelected(ll_Fila) Then
		ls_Busca = 'paen_numero = ' + String(dw_3.Object.paen_numero[ll_Fila])
		
		ll_Busca = dw_1.Find(ls_Busca, 1, dw_1.RowCount(), Primary!)
		
		If ll_Busca = 0 Then
			
			If dw_1.RowCount() > 0 Then
				If (Date(dw_2.Object.rfpe_fecrec[dw_2.GetRow()]) <> Date(dw_1.Object.rfpe_fecrec[1])) Then
					MessageBox('ERROR', 'Folios con distinta fehca de recepcion', StopSign!, OK!)
					Return
				End If
			ENd If
		
			ll_New = dw_1.InsertRow(0)
			
			dw_1.Object.clie_codigo[ll_New]		= dw_3.Object.clie_codigo[ll_Fila]
			dw_1.Object.plde_codigo[ll_New]		= dw_3.Object.plde_codigo[ll_Fila]
			dw_1.Object.rfpe_numero[ll_New]	= dw_3.Object.rfpe_numero[ll_Fila]
			dw_1.Object.rfpe_fecrec[ll_New]		= dw_2.Object.rfpe_fecrec[dw_2.GetRow()]
			dw_1.Object.rfpe_nrores[ll_New] 		= dw_2.Object.rfpe_nrores[dw_2.GetRow()]
			dw_1.Object.paen_numero[ll_New] 	= dw_3.Object.paen_numero[ll_Fila]
		End If
	End If
Next

CloseWithReturn(This, istr_mant)
end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_traspasorecepcion
boolean visible = false
boolean enabled = false
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_traspasorecepcion
boolean visible = false
boolean enabled = false
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_traspasorecepcion
boolean visible = false
boolean enabled = false
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_traspasorecepcion
boolean visible = false
boolean enabled = false
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_traspasorecepcion
boolean visible = false
integer x = 3150
integer y = 400
boolean enabled = false
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_traspasorecepcion
integer x = 3150
integer y = 156
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_traspasorecepcion
boolean visible = false
integer x = 3150
integer y = 652
boolean enabled = false
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_traspasorecepcion
boolean visible = false
integer x = 59
integer y = 116
integer width = 2917
integer height = 960
boolean enabled = false
string dataobject = "dw_mues_traspasocliente"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_columna, ls_nula
Integer  li_lote
Long		ll_Prod
SetNull(ls_Nula)

ls_columna = dwo.name


CHOOSE CASE ls_columna
		
	CASE "emba_codigo"
		istr_mant.Argumento[7]=Data

	CASE "vari_codigo"
		istr_mant.Argumento[4]=Data
			
	CASE "prod_codigo"
		IF  NoExisteProductor(data) THEN
			dw_1.SetItem(il_fila, ls_columna, Long(ls_Nula))
			RETURN 1
			
		ELSEIF istr_mant.Argumento[20]='1' THEN

				dw_1.GetChild("pafr_huert1", idwc_predio)
				idwc_predio.SetTransObject(SQLCA)
				ll_Prod	=	Long(data)
				IF idwc_predio.Retrieve(ll_Prod) = 0 THEN
					idwc_predio.InsertRow(0)
				END IF
				
				IF Varificaproductor(Integer(Data))=FALSE THEN
					RETURN 1
				END IF
						
		END IF
		
	CASE "pafr_huert1" 
		
		IF Not noexistepredio(dw_1.Object.Prod_codigo[row],integer(data)) THEN
			dw_1.SetItem(row, "pafr_huert1", integer(ls_nula))
			Return 1
		ELSE
			dw_1.GetChild("pafr_cuart1", idwc_cuartel)
			idwc_cuartel.SetTransObject(SQLCA)
			idwc_cuartel.Retrieve(dw_1.Object.Prod_codigo[row],integer(data))
			
		END IF
		
	CASE "pafr_calibr"
		IF NoExisteCalibre(data) OR Duplicado(Upper(data)) THEN
			dw_1.SetItem(il_fila, ls_columna, Upper(ias_campo[12]))
			RETURN 1
		END IF

	CASE "pafr_ccajas"
			IF Long(data) < 0 THEN
				dw_1.SetItem(il_fila, ls_columna, Long(istr_mant.Argumento[11]))
				RETURN 1
			END IF
			
			IF Long(data) > Long(istr_mant.Argumento[11]) THEN
				MessageBox("Atención", "Cajas ingresadas sobrepasan las " + istr_mant.Argumento[11] + &
								" cajas del Pallet")
				dw_1.SetItem(il_fila, ls_columna, Long(istr_mant.Argumento[11]))
				RETURN 1
			END IF
		CASE "pafr_fecing"
				IF NoExisteFechaIngreso(data) THEN
					dw_1.SetItem(il_fila, ls_columna,Date( ls_Nula))
					RETURN 1
				END IF
		CASE "pafr_fecemb"
				IF NoExisteFechaEmbalaje(data) THEN
					dw_1.SetItem(il_fila, ls_columna, Date(ls_Nula))
					RETURN 1
				END IF
END CHOOSE
end event

event dw_1::clicked;call super::clicked;String	ls_columna
ls_columna = dwo.name

CHOOSE CASE ls_columna
	CASE "buscaprod"
		buscaprod()
	
END CHOOSE
end event

type dw_2 from uo_dw within w_mant_deta_traspasorecepcion
integer x = 59
integer y = 116
integer width = 2917
integer height = 960
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_recfruprocee"
end type

event clicked;call super::clicked;If Row > 0 Then
	il_Fila	=	Row
	
	This.SelectRow(0, False)
	This.SelectRow(il_fila, True)
	This.SetRow(il_fila)
End If
end event

event doubleclicked;call super::doubleclicked;Long	ll_Fila, Respuesta

DO
	dw_3.SetRedraw(False)
	dw_3.Reset()
	ll_Fila	=	dw_3.Retrieve(This.Object.plde_codigo[Row], This.Object.rfpe_numero[Row], This.Object.clie_codigo[Row])
	
	IF ll_Fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, OK!)
	END IF
	dw_3.SetRedraw(True)
LOOP WHILE respuesta = 1

end event

type dw_3 from uo_dw within w_mant_deta_traspasorecepcion
integer x = 78
integer y = 1112
integer width = 3415
integer height = 1000
integer taborder = 21
boolean bringtotop = true
boolean titlebar = true
string title = "Detalle de Folios"
string dataobject = "dw_mues_recfruproced"
end type

event clicked;call super::clicked;String	ls_old_sort, ls_column
Char	lc_sort

IF IsNull(dwo) THEN RETURN

If Right(dwo.Name,2) = '_t' Then
	ls_column = Left (dwo.Name, Len(String(dwo.Name)) - 2)
	ls_old_sort = This.Describe("Datawindow.Table.sort")

	If ls_column = Left(ls_old_sort, Len(ls_old_sort) - 2) Then
		lc_sort = Right(ls_old_sort, 1)
		If lc_sort = 'A' Then
			lc_sort = 'D'
		Else
			lc_sort = 'A'
		End If
		This.SetSort(ls_column+" "+lc_sort)
	Else
		This.SetSort(ls_column+" A")
		This.Modify(Left(ls_old_sort, Len(ls_old_sort) - 2) + "_t.Color = " + String(Rgb(255,255,255)))
	End If
	
	This.Modify(dwo.Name + ".Color = " + String(Rgb(255, 255, 0)))
	
	This.Sort()
End If

IF Row > 0 THEN
	This.SetRow(Row)
	If This.IsSelected(Row) Then 
		This.SelectRow(Row,False)
	Else
		This.SelectRow(Row,True)
	End If
END If
end event

